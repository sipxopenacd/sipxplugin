-module(spx_cdr_mongo).
-behavior(gen_cdr_dumper).

-include_lib("openacd/include/call.hrl").
-include_lib("openacd/include/agent.hrl").

-export([
	init/1,
	terminate/2,
	code_change/3,
	dump/2,
	commit/1,
	rollback/1
]).

-record(state, {
	agent_cache = dict:new()
}).

init(_Opts) ->
	{ok, #state{}}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

dump(AgState, State) when is_record(AgState, agent_state) ->

	#agent_state{
		id = AgentId,
		agent = Agent,
		state = Ztate,
		oldstate = OldZtate,
		start = Start,
		ended = End,
		profile = Profile
	} = AgState,

	Cache = State#state.agent_cache,
	{Clients, Cache1} = get_clients(Agent, Cache),

	Entry = [
		{<<"vsn">>, <<"0.1.0">>},
		{<<"agent">>, fx(Agent)},
		{<<"agent_id">>, fx(AgentId)},
		{<<"profile">>, fx(Profile)},
		{<<"clients">>, {array, Clients}},
		{<<"old_state">>, fx(OldZtate)},
		{<<"state">>, fx(Ztate)},
		{<<"start">>, as_erl_now(Start)},
		{<<"end">>, as_erl_now(End)}
	],

	try_insert(<<"asl">>, Entry),

	{ok, State#state{agent_cache=Cache1}};
dump(CDR, State) when is_record(CDR, cdr_rec) ->
	Call = CDR#cdr_rec.media,
	Summary = CDR#cdr_rec.summary,
	Raws = CDR#cdr_rec.transactions,

	Agent = find_agent(Summary),
	Queue = find_queue(Summary),

	%% WARN: might not be the active profile when the call was made
	Profile = get_profile(Agent),

	CallID = Call#call.id,
	Client = (Call#call.client)#client.label,

	TRs = [{Tr, St} || #cdr_raw{transaction=Tr, start=St}<-Raws, is_atom(Tr)],

	Entry = [
		{<<"vsn">>, <<"0.1.0">>},
		{<<"agent">>, fx(Agent)},
		{<<"profile">>, fx(Profile)},
		{<<"queue">>, fx(Queue)},
		{<<"client">>, fx(Client)},
		{<<"callid">>, fx(CallID)},
		{<<"transactions">>,
			{array, [[{<<"state">>, fx(Tr)},
				{<<"ts">>, as_erl_now(St)}] || {Tr, St} <- TRs]}}
	],
	K = mongoapi:new(spx, <<"openacd">>),
	K:insert(<<"cdr">>, Entry),

	try_insert(<<"cdr">>, Entry),
	{ok, State}.

commit(State) ->
	{ok, State#state{agent_cache=dict:new()}}.

rollback(State) ->
	{ok, State#state{agent_cache=dict:new()}}.

%% Internal
find_agent(Summary) ->
	case proplists:get_value(oncall, Summary) of
		{_, [{Ag, _}|_]} ->
			Ag;
		_ ->
			null
	end.

find_queue(Summary) ->
	case proplists:get_value(inqueue, Summary) of
		{_, [{Q, _}|_]} ->
			Q;
		_ ->
			null
	end.

get_profile(null) -> null;
get_profile(Ag) ->
	case catch agent_auth:get_agent_by_login(Ag) of
		{ok, #agent_auth{profile=P}} -> P;
		_ -> null
	end.


as_erl_now(S) when is_integer(S) ->
	{S div 1000000, S rem 1000000, 0};
as_erl_now(_) ->
	null.

fx(undefined) -> null;
fx(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
fx(Othr) -> Othr.

get_clients(Login, Cache) ->
	{Ag, Cache1} = get_agent(Login, Cache),
	Clients = case Ag of
		#agent_auth{skills=Skills} when is_list(Skills) ->
			[Cl || {'_brand', Cl} <- Skills];
		_ ->
			[]
	end,
	{Clients, Cache1}.

get_agent(Login, Cache) ->
	case dict:find(Login, Cache) of
		{ok, Ag} ->
			{Ag, Cache};
		_ ->
			E = case catch agent_auth:get_agent_by_login(Login) of
				{ok, E1} -> E1;
				_ -> null
			end,
			{E, dict:store(Login, E, Cache)}
	end.

try_insert(Tbl, Entry) ->
	K = mongoapi:new(spx, <<"openacd">>),
	case catch K:insert(Tbl, Entry) of
		ok -> ok;
		Other -> lager:warning("Not able to insert to mongo records due to ~p: ~p", [Other, Entry])
	end.

