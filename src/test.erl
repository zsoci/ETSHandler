%% @author zsoci
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([all/0, mod/1]).

-define(MODULES,[etsserver]).

all() ->
	case application:start(multiserver) of
		ok ->
			testmodules(?MODULES),
			case application:stop(multiserver) of
				ok ->
					ok;
				WAFIT ->
					io:format("Error stopping application: ~p",[WAFIT])
			end;
		WAFIT ->
			io:format("Error starting application: ~p",[WAFIT])
	end
.

mod(Module) ->
	testmodules([Module])
.
%% ====================================================================
%% Internal functions
%% ====================================================================

testmodules([]) -> ok;
testmodules([Module|Rest]) ->
	Module:test(),
	testmodules(Rest)
.