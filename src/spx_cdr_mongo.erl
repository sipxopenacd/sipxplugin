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

init(_Opts) ->
	{ok, []}.

terminate(_Reason, _State) ->
	ok.

code_change(_Oldvsn, State, _Extra) ->
	{ok, State}.

dump(AgState, State) when is_record(AgState, agent_state) ->
	lager:info("ag state: ~p", [AgState]),
	{ok, State};
dump(CDR, State) when is_record(CDR, cdr_rec) ->
	Call = CDR#cdr_rec.media,
	Summary = CDR#cdr_rec.summary,
	Raws = CDR#cdr_rec.transactions,

	Agent = find_agent(Summary),
	Queue = find_queue(Summary),

	CallID = Call#call.id,
	Client = (Call#call.client)#client.label,

	TRs = [{Tr, St} || #cdr_raw{transaction=Tr, start=St}<-Raws, is_atom(Tr)],

	Entry = [
		{<<"vsn">>, <<"0.1.0">>},
		{<<"agent">>, Agent},
		{<<"queue">>, Queue},
		{<<"client">>, Client},
		{<<"callid">>, CallID},
		{<<"transactions">>,
			{array, [[{<<"state">>, atom_to_binary(Tr, utf8)},
				{<<"ts">>, as_erl_now(St)}] || {Tr, St} <- TRs]}}
	],
	K = mongoapi:new(spx, <<"openacd">>),
	K:insert(<<"cdr">>, Entry),

	{ok, State}.

commit(State) ->
	{ok, State}.

rollback(State) ->
	{ok, State}.

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

as_erl_now(S) ->
	{S div 1000000, S rem 1000000, 0}.

