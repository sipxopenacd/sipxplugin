-module(spx_dialplan_loader).

-export([start/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start() ->
	spx_db:connect(),

	ActionFun = fun get_action/1,
	ReloadFun = fun reload/1,
	spx_autoloader:add_mod({?MODULE, ActionFun, undefined, undefined, ReloadFun}, none).

get_action(OldConf) ->
	M = mongoapi:new(spx, <<"imdb">>),

	case M:findOne(<<"entity">>, [{<<"_id">>, <<"OpenAcdAgentConfigCommand-1">>}]) of
		{ok, Prop} ->
			case ej:get({"autologout"}, Prop) of
				OldConf ->
					none;
				Conf when is_integer(Conf) ->
					{reload, Conf}
			end;
		_ ->
			none
	end.

reload(Conf) ->
	lager:info("Setting dialplan autologout to ~p ms", [Conf * 1000]),
	oacd_dialplan_listener:set_timeout(Conf * 1000).

-ifdef(TEST).

-endif.
