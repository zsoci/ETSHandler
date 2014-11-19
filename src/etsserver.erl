%% @author zsoci
%% @doc @todo Add description to atomdb.


-module(etsserver).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define (LOGFORMAT(_Level,Format,Args),io:format(Format,Args)).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0,initdb/0,start_link/0,newtable/1,showtables/0,die/0]).

-define(ETSDB,"etstables.edb").
-define(DETSTABLENAME,etstables).

start_link() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).

newtable(TableName) -> utils:call_server(?MODULE, {new_table,TableName}).
showtables() -> utils:call_server(?MODULE, showtables).
die() -> gen_server:cast(?MODULE, die).

initdb() ->
	case dets:open_file(?DETSTABLENAME,[{file,?ETSDB}]) of
		{ok, Table} ->
			dets:delete_all_objects(Table),
			dets:close(Table);
		WAFIT -> WAFIT
	end
.

test() ->
	newtable(ets_test1),
	newtable(ets_test2),
	die(),
	timer:sleep(2000),
	etshandler:die(ets_test1),
	timer:sleep(2000),
	showtables()
.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%%-record(state, {tables = undefined}).

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
	?LOGFORMAT(debug,"Etsserver init",[]),
	case dets:open_file(?ETSDB) of
		{ok,DETSTable} ->
			dets:traverse(DETSTable,fun ({TableName,TableId}) ->
											 etshandler:reheir(TableName,TableId,self()),
											 continue
						  end),
			dets:close(DETSTable),
			{ok,undefined};
		{error,Reason} ->
			?LOGFORMAT(error,"Could not open ets db file:~p\nReason:~p\n",[?ETSDB,Reason]),
			{stop,badfile}
	end
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
handle_call({new_table,TableName}, _From, State) ->
	case dets:open_file(?ETSDB) of
		{ok,DETSTable} ->
			case dets:lookup(DETSTable, TableName) of
				[] -> % table is not yet alive so let us create it
					TId = ets:new(TableName, [bag,{heir,self(),TableName}]),
					ETSWorker = {TableName,{etshandler,start_link,[TableName]},
					  			 permanent,2000,worker,[etshandler]},
					case supervisor:start_child(multiserver_sup, ETSWorker) of
						{ok, WorkerPId} ->
							case give_away(TableName,TId) of
								{ok, PId} ->
									if
										WorkerPId =:= PId ->
											Reply = dets:insert_new(DETSTable, {TableName,TId});
										true ->
											Reply = false
									end;
								{error,Reason} ->
									?LOGFORMAT(error,"Could not give the table ~p to worker ~p\nReason:~p\n",[TableName,WorkerPId,Reason]),
									Reply = false
							end;
						WAFIT ->
							?LOGFORMAT(error,"Could not start ets worker for table:~p\nReason:~p\n",[TableName,WAFIT]),
							Reply = false
					end;
				WAFIT ->
					?LOGFORMAT(error,"Table already created:~p\nReason:~p\n",[TableName,WAFIT]),
					Reply = false
			end,
			case dets:close(DETSTable) of
				ok ->
					Reply2 = Reply;
				Reason2 ->
					Reply2 = Reason2
			end;
				  
		WAFIT ->
			?LOGFORMAT(error,"Table db cannot be opened. Reason:~p\n",[WAFIT]),
			Reply2= false
	end,
	{reply, Reply2, State}
;

handle_call(showtables, _From, State) ->
	case dets:open_file(?ETSDB) of
		{ok,DETSTable} ->
			?LOGFORMAT(?ETSLOGLEVEL,"The following ets tables exist:\n",[]),
			dets:traverse(DETSTable,fun ({TableName,TableId}) ->
											 ?LOGFORMAT(debug,"Table:~p,Id:~p\n",[TableName,TableId]),
											 continue
						  end),
			dets:close(DETSTable),
			Reply = ok;
		{error,Reason} ->
			?LOGFORMAT(error,"Could not open ets db file:~p\nReason:~p\n",[?ETSDB,Reason]),
			Reply = ok
	end,
    {reply, Reply, State}
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
handle_cast(die,State) ->
%	exit(killed),
	erlang:error(badarg),
	{noreply,State}
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
handle_info({'ETS-TRANSFER',TableId,FromPid,TableName}, State) ->
	?LOGFORMAT(?ETSLOGLEVEL,"Got the table ownership back for table:~p from ~p. Table name is :~p",[TableId,FromPid,TableName]),
	give_away(TableName,TableId),
    {noreply, State}
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
	?LOGFORMAT(error,"etsserver terminated for reason:~p",[Reason]),
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
	?LOGFORMAT(?ETSLOGLEVEL,"etsserver code change. OldVsn:~p\nState:~p\nExtra:~p",[OldVsn,State,Extra]),
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

wait_for_handler(TableName,Timeout) ->
	case whereis(TableName) of
		undefined ->
			case Timeout of
				infinity ->
					timer:sleep(10),
					wait_for_handler(TableName, Timeout);
				Time when Time > 0 ->
					timer:sleep(10),
					wait_for_handler(TableName, Time - 10);
				_ ->
					timeout
			end;
		Pid ->
			Pid
	end
.

give_away(TableName,TId) ->
	case wait_for_handler(TableName,infinity) of
		timeout ->
			{error,timeout};
		PId when is_pid(PId)->
			ets:give_away(TId, PId, TableName),
			{ok,PId};
		WAFIT ->
			?LOGFORMAT(error,"Could not wait for ets worker for table:~p\nReason:~p\n",[TableName,WAFIT]),
			{error,WAFIT}
	end
.
