%% @author zsoci
%% @doc @todo Add description to bridgeserver_app.


-module(multiserver_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([multiserver_start/0]).
%% -include("multiserver.hrl"). 
%% -include("../../logger/include/logger.hrl").
-define (LOGFORMAT(_Level,Format,Args),io:format(Format,Args)).


multiserver_start() -> application:start(multiserver).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, StartArgs) ->
%% 	case net_kernel:connect_node(utils:get_env(logserver)) of
%% 		true ->
%% 			global:sync(),
%% 			logserver:clearlog(),
%% %			logserver:register(?LOGNAME, lists:append([utils:get_env(logpath),utils:get_env(logfilename)]),utils:get_env(loglevels)),
			etsserver:initdb(),
			case multiserver_sup:start_link(StartArgs) of
				{ok, Pid} ->
					{ok, Pid};
				Error ->
					{error,Error}
			end
%% 		A -> 
%% %%			?LOGFORMAT(error,"Log server cannot be connected:~p",[A]),
%% 			{error,nologserver}
%% 	end
.
%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
%	application:stop(inets),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


