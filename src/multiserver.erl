%% @author zsoci
%% @doc @todo Add description to multiserver.


-module(multiserver).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% -include("../../logger/include/logger.hrl").
%% -include("multiserver.hrl").
-define (LOGFORMAT(_Level,Format,Args),io:format(Format,Args)).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,startup/0,getwebserverpid/0]).

start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
startup() -> gen_server:cast(?MODULE, startup).
getwebserverpid() -> utils:call_server(?MODULE, getwebserverpid).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	process_flag(trap_exit, true),
	startup(),
	{ok, #state{}}
.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(getwebserverpid, _From, State) ->
    {reply, get(httpserverpid), State}
;



handle_call(Request, From, State) ->
	?LOGFORMAT(error,"Got an unmached call request from PID:~p. Request=~p\n",[From,Request]),
    Reply = ok,
    {reply, {ok,Reply}, State}
.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================


handle_cast(startup, State) ->
 	?LOGFORMAT(debug,"Got a startup message.",[]),
	ETSServer = {etsserver,{etsserver,start_link,[]},
				 permanent,2000,worker,[etsserver]},
	case supervisor:start_child(multiserver_sup, ETSServer) of
		{ok, _ServerPId} ->
  			inets:start(),  %% start from config file multiserver.config
%			case inets:start(httpd,[{proplist_file,utils:get_env(httpconfigfilename)}]) of
			case lists:keyfind(httpd, 1, inets:services()) of
				{httpd,Pid} ->
					put(httpserverpid, Pid),
					?LOGFORMAT(debug,"Web server started. Pid:~p",[Pid]);
					% create ets tables for running
%% 					etsserver:newtable(?LOCALACTORETS),
%% 					etsserver:newtable(?NODECONTROLLERETS);
				WAFIT ->
					?LOGFORMAT(debug,"Web server error when starting:~p",[WAFIT])
			end;
		WAFIT ->
			?LOGFORMAT(error,"Could not start etsserver:~p\n",[WAFIT])
	end,
	{noreply, State}
;

handle_cast(Msg, State) ->
 	?LOGFORMAT(error,"Got an unmached cast request:~p.",[Msg]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
	?LOGFORMAT(error,"Got an unmached handle_info request:~p\n",[Info]),
     {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, _State) ->
	?LOGFORMAT(error,"multiserver terminated for reason:~p",[Reason]),
     ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
	?LOGFORMAT(debug,"multiserver code change. OldVsn:~p\nState:~p\nExtra:~p",[OldVsn,State,Extra]),
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


