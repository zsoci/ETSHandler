%% @author zsoci
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([all/0, mod/1,log/0]).

-define(MODULES,[etsserver]).
-define(LOGFILE,lists:append([utils:get_env(multiserver,logpath),utils:get_env(multiserver,logfilename)])).

all() ->
	case application:start(multiserver) of
		ok ->
			LogFile = ?LOGFILE,
			testmodules(?MODULES),
			case application:stop(multiserver) of
				ok ->
					ok;
				WAFIT ->
					io:format("Error stopping application: ~p",[WAFIT])
			end,
			print_file(LogFile);
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

log() ->
	print_file(?LOGFILE)
.

testmodules([]) -> ok;
testmodules([Module|Rest]) ->
	Module:test(),
	testmodules(Rest)
.

print_file(FileName) ->
	case file:open(FileName, [read]) of
		{ok,Device} ->
			print_lines(Device);
		WAFIT ->
			io:format("Error opening log file:~p\nReason:~p",[FileName,WAFIT])
	end
.

print_lines(Device) ->
	case io:get_line(Device, "") of
		eof ->
			file:close(Device);
		Line ->
			io:format("~s",[Line]),
			print_lines(Device)
	end
.
	