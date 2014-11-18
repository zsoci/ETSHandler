%% @author zsoci
%% @doc @todo Add description to bridgeserver_sup.


-module(multiserver_sup).
-behaviour(supervisor).
-export([init/1]).

%% -include("multiserver.hrl"). 
%% -include("../../logger/include/logger.hrl").
-define (LOGFORMAT(_Level,Format,Args),io:format(Format,Args)).
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
%	case setupserver() of
%		ok -> 
			MainServer = {multiserver,{multiserver,start_link,[]},
					  	permanent,2000,worker,[multiserver]},
			{ok,{{one_for_one,3,10}, [MainServer]}}
%		_ -> ignore
%	end 
.
%% ====================================================================
%% Internal functions
%% ====================================================================

setupserver() ->
%			application:start(inets),
			inets:start(),
			A = inets:start(httpd,[{proplist_file,"D:/Develop/Luna/BridgeServer/12001_props.conf"}]),
			?LOGFORMAT(debug,"Web server started:~p",[A])
%% 			A = inets:start(httpd,[{port,utils:get_env(serverport)},
%% 								   {server_name,"bridgeserver"},
%% 								   {server_root,"serverroot"},
%% 								   {document_root,"docroot"},
%% 								   {modules,[mod_log,webserver]},
%% 								   %%										   {re_write, {"^/BGCModule.asmx", "D:/Develop/Eclipse/Bridgeserver/esi/my_esi/ws"}},
%% 								   %%										   {alias, {"/BGCModule.asmx", "/esi/my_esi/ws"}},
%% 								   {erl_script_alias,{"/esi",[my_esi,io]}},
%% 								   %%										   {log_format, pretty},
%% 								   {error_log,"errorlog.txt"},
%% 								   {security_log,"securitylog.txt"},
%% 								   {transfer_log,"transferlog.txt"},
%% 								   %%					   {ipfamily,inet},
%% 								   {allow_from,all},
%% 								   {bind_address,{192,168,0,100}}]),
.
