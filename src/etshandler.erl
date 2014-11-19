%% @author zsoci
%% @doc @todo Add description to etshandler.


-module(etshandler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,stop/1,getid/1,store/2,start_link/1,reheir/3,die/1,
		 load/2,delete/2]).

start(TableName) -> start_link(TableName).
stop(TableName) -> gen_server:call(TableName,stop,infinity).
getid(TableName) -> gen_server:call(TableName, getid, infinity).
reheir(TableName,TableId,HeirId) -> utils:call_server(TableName, {reheir,TableName,TableId,HeirId}).
start_link(TableName) -> gen_server:start_link({local,TableName},?MODULE,[],[]).
store(TableName,Object) -> utils:call_server(TableName, {store,TableName,Object}).
load(TableName,Key) -> utils:call_server(TableName, {load,TableName,Key}).
delete(TableName,Key) -> utils:call_server(TableName, {delete,TableName,Key}).
die(TableName) -> gen_server:cast(TableName, die).
delayed_store(TableName,Object,From) -> gen_server:cast(TableName, {delayed_store,TableName,Object,From}).
delayed_load(TableName,Key,From) -> gen_server:cast(TableName, {delayed_load,TableName,Key,From}).
delayed_delete(TableName,Key,From) -> gen_server:cast(TableName, {delayed_delete,TableName,Key,From}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

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
    {ok, undefined}
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
handle_call(stop, _From, State) ->
	{stop,normal,stopped,State}
;

handle_call(getid, _From, State) ->
	{reply,State,State}
;

handle_call({reheir,TableName,TableId,HeirPid}, _From, State) ->
	?LOGFORMAT(?ETSLOGLEVEL,"~p got a reheir request to ~p for table ~p\n",[TableName,HeirPid,TableId]),
	ets:setopts(TableId, {heir,HeirPid,TableName}),
	{reply,State,State}
;

handle_call({store,TableName,Object}, From, State) ->
	case State of
		undefined ->
			delayed_store(TableName,Object,From),
			{noreply,State};
		TId ->
			ets:insert(TId, Object),
			{reply, ok, State}
	end
;

handle_call({load,TableName,Key}, From, State) ->
	case State of
		undefined ->
			delayed_load(TableName,Key,From),
			{noreply,State};
		TId ->
			Reply = ets:lookup(TId, Key),
			{reply, Reply, State}
	end
;

handle_call({delete,TableName,Key}, From, State) ->
	case State of
		undefined ->
			delayed_delete(TableName,Key,From),
			{noreply,State};
		TId ->
			Reply = ets:delete(TId, Key),
			{reply, Reply, State}
	end
;
			
handle_call(Request, From, State) ->
	?LOGFORMAT(error,"~p got an unmached request from PID:~p. Request=~p\n",[State,From,Request]),
    Reply = ok,
    {reply, Reply, State}
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
handle_cast(die,State) ->
%	exit(killed),
	?LOGFORMAT(error,"Got a die request\n",[]),
	erlang:error(badarg),
	{noreply,State}
;

handle_cast({delayed_store,TableName,Object,From},State) ->
	case State of
		undefined ->
			delayed_store(TableName,Object,From);
		TId ->
			ets:insert(TId, Object),
			gen_server:reply(From, ok)
	end,
	{noreply, State}
;

handle_cast({delayed_load,TableName,Key,From},State) ->
	case State of
		undefined ->
			delayed_load(TableName,Key,From);
		TId ->
			Reply = ets:lookup(TId, Key),
			gen_server:reply(From, Reply)
	end,
	{noreply, State}
;

handle_cast({delayed_delete,TableName,Key,From},State) ->
	case State of
		undefined ->
			delayed_load(TableName,Key,From);
		TId ->
			Reply = ets:delete(TId, Key),
			gen_server:reply(From, Reply)
	end,
	{noreply, State}
;

handle_cast(Msg, State) ->
	?LOGFORMAT(error,"Got an unmached handle_cast request:~p\n",[Msg]),
	{noreply, State}
.


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


handle_info({'ETS-TRANSFER',Tab,_FromPid,TableName}, State) ->
	?LOGFORMAT(?ETSLOGLEVEL,"Got the table ownership for table:~p. It was:~p",[TableName,State]),
    {noreply, Tab}
;

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
	?LOGFORMAT(debug,"etshandler terminated for reason:~p\n",[Reason]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	?LOGFORMAT(?ETSLOGLEVEL,"etshandler code change request\n",[]),
     {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


