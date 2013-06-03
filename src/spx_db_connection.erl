-module(spx_db_connection).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([check_connection/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(CONNECTION_CHECK_INTERVAL, 5000).
-define(DB, spx).

-record(state, {check_timer}).

%% API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec check_connection() -> boolean().
check_connection() ->
	gen_server:call(?MODULE, check_connection).

%% gen_server callbacks
init([]) ->
	{_, Timer} = try_check_connection(),
	{ok, #state{check_timer = Timer}}.

handle_call(check_connection, _From, State) ->
	{Reply, Timer} = try_check_connection(),
	{reply, Reply, State#state{check_timer = Timer}};
handle_call(_, _From, State) ->
	{reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
-spec try_check_connection() -> {boolean(), undefined | reference()}.
try_check_connection() ->
	case catch mongodb:is_connected(?DB) of
		true ->
			lager:info("DB ~p is connected", [?DB]),
			{true, undefined};
		_ ->
			spx_db:connect(),
			{ok, Timer} = timer:apply_after(?CONNECTION_CHECK_INTERVAL, ?MODULE,
				check_connection, []),
			{false, Timer}
	end.
