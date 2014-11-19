%% @author zsoci
%% @doc @todo Add description to bridgeserver_sup.


-module(multiserver_sup).
-behaviour(supervisor).
-export([init/1]).

%% -include("multiserver.hrl"). 
%% -include("../../logger/include/logger.hrl").
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,start_in_shell_for_testing/0,start/0]).

start() ->
	spawn(fun() -> 
				  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
		  end)
.

start_in_shell_for_testing() ->
	{ok,Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid)
.

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args)
.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
	MainServer = {multiserver,{multiserver,start_link,[]},
				  permanent,2000,worker,[multiserver]},
	{ok,{{one_for_one,3,10}, [MainServer]}}
.

%% ====================================================================
%% Internal functions
%% ====================================================================
