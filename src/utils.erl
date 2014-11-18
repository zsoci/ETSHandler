%% @author zsoci
%% @doc global utility functions
%% @todo Add description to utils.

-module(utils).
-define(DEFAULTRETRY,5).
-define(DEFAULTSLEEP,1).
-define(DEFAULTTIMEOUT,infinity).

%% ====================================================================
%% API functions
%% ====================================================================
-export([call_server/2,call_server/3,call_server/4,call_server/5]).

%%-include("../../logger/include/logger.hrl").
%%-include("multiserver.hrl").
-define (LOGFORMAT(_Level,Format,Args),io:format(Format,Args)).
%% %% get_env/1
%% -spec get_env(Key :: atom()) -> Result when
%% 	Result :: term()
%% 			| undefined
%% .
%% %% <a>Returns a value for the key from the application profile</a>
%% get_env(Key) ->
%% 	case application:get_env(Key) of
%% 		{ok,Value} -> Value;
%% 		Any -> Any
%% 	end
%% .
%% 
%% %% get_env/2
%% %% <a>Returns a value for the key from the given application's profile</a>
%% -spec get_env(App :: atom(), Key :: atom()) -> Result when
%% 	Result :: term()
%% 			| undefined
%% .
%% get_env(App,Key) ->
%% 	case application:get_env(App,Key) of
%% 		{ok,Value} -> Value;
%% 		Any -> Any
%% 	end
%% .

%% call_server/2
%% ====================================================================
%% Call a gen server with a ServerName and Arguments with timeout : {@?DEFAULTTIMEOUT}, retry : {@DEFAULTRETRY}, sleep : {@DEFAULTSLEEP}
-spec call_server(ServerName :: atom(), Arguments :: term()) -> Result when
	Result :: term()
.
% ====================================================================
call_server(Server,Arguments) -> call_server(Server,Arguments,?DEFAULTTIMEOUT,?DEFAULTRETRY,?DEFAULTSLEEP,?DEFAULTRETRY).

%% call_server/3
%% ====================================================================
%% @doc <a>Call a gen server with a ServerName, Arguments, Retry count, default timeout : {@DEFAULTTIMEOUT}, default sleep time : {@DEFAULTSLEEP}</a>
-spec call_server(ServerName :: atom(), Arguments :: term(), Retry :: integer()) -> Result when
	Result :: term()
.
% ====================================================================
call_server(Server,Arguments,Retry) -> call_server(Server,Arguments,?DEFAULTTIMEOUT,Retry,?DEFAULTSLEEP,Retry).

%% call_server/4
%% ====================================================================
%% @doc <a>Call a gen server with a ServerName, Arguments, Retry count, Sleep time and default timeout : {@DEFAULTTIMEOUT}</a>
-spec call_server(ServerName :: atom(), Arguments :: term(), Retry :: integer(), Sleep :: integer()) -> Result when
	Result :: term()
.
% ====================================================================
call_server(Server,Arguments,Retry,Sleep) -> call_server(Server, Arguments, ?DEFAULTTIMEOUT, Retry, Sleep, Retry).

%% call_server/5
%% ====================================================================
%% @doc <a>Call a gen server with a ServerName, Arguments, Retry count, Sleep time and Timeout</a>
-spec call_server(ServerName :: atom(), Arguments :: term(), Retry :: integer(), Sleep :: integer(), Timeout :: integer() | infinity) -> Result when
	Result :: term()
.
% ====================================================================
call_server(Server,Arguments,Retry,Sleep,Timeout) -> call_server(Server,Arguments,Timeout,Retry,Sleep,Retry).

%% hostname/0
%% Returns the current host's fully qualified name
%% -spec hostname() -> string().
%% hostname() ->
%%     case {inet_db:gethostname(),inet_db:res_option(domain)} of
%%         {H,D} when is_list(D), is_list(H),
%%                    length(D)> 0, length(H)>0 ->
%%             H ++ "." ++ D;
%%         Other ->
%%             error({hostname, Other})
%%     end.
%% 
%% %% ipadresses/1
%% %% Returns all the ip addresses of the currnet hosts's network interfaces
%% -spec ipaddress(Family :: inet | inet6 | inet6fb4) -> string().
%% ipaddress(Family) ->
%% 	{ok,HostName} = inet:gethostname(),
%% 	case inet:getaddr(HostName, Family) of
%% 		{error, PosixError} ->
%% 			?LOGFORMAT(error,"No ip address found. posix: ~p",[PosixError]),
%% 			error;
%% 		{ok,Address} ->
%% %			io:format("IFList:~p\n", Address),
%% 			inet_parse:ntoa(Address)
%% 	end
%% .
%% %% ipadresses/0
%% %% Returns ip addresses of the currnet hosts's network interfaces for inet family
%% -spec ipaddress() -> string().
%% ipaddress() -> ipaddress(inet).
%% %% ====================================================================
%% %% Internal functions
%% %% ====================================================================
%% %% call_server/6
%% ====================================================================
%% @doc <a>Call a gen server with a ServerName, Arguments, Retry count, Sleep time and Timeout</a>
-spec call_server(ServerName :: atom(), Arguments :: term(), Timeout:: integer() | infinity, RetryCount :: integer(), Sleep :: integer(), Count :: integer() | infinity) -> Result when
	Result :: term()
.

% ====================================================================

call_server(Server,Arguments,Timeout,RetryCount,Sleep,Count) ->
	try
		gen_server:call(Server, Arguments,Timeout)
	catch
		A:B -> 
			?LOGFORMAT(error,"Exception:~p:~p",[A,B]),
			?LOGFORMAT(error,"Stack:~p",[erlang:get_stacktrace()]),
			if 
				Count =:= 0 ->
					?LOGFORMAT(error,"Could not call server ~p with ~p after ~p retries.",[Server,Arguments,RetryCount]),
					error;
				true ->
					?LOGFORMAT(error,"Could not call server ~p with ~p.\nRetry:~p",[Server,Arguments,Count]),
					timer:sleep(Sleep),
					call_server(Server,Arguments,Timeout,RetryCount,Sleep,Count - 1)
			end

	end
.


