ETSHandler
===========
Example for application that makes sure you do not lose your ETS tables.

Steve Vinoski wrote an excellent article about the subject.
http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/
http://steve.vinoski.net/blog/2013/05/08/implementation-of-dont-lose-your-ets-tables/

He also states "The Erlang supervisor creates a table manager process. Since all this process does is manage the table, the likelihood of it crashing is very low."
And he is right. Anyway likelihood is low means it can happen.

For experiment and playing around with ETS tables, there is a way to make this likelihood even lower. The code handles basic ETS operations, store, load and delete. Can be further developed as you wish.

#The idea

The goal is not to lose ETS table as long as the node is alive. Having a process owning the table is a scenario where we can lose the table when that process dies for any reason, even if a supervisor restarts it, ETS is gone.

There is a way to pass ETS table ownership when a process dies. The option heir and the function ets:give_away/3 do the job and are documented on http://www.erlang.org/doc/man/ets.html#give_away-3

Using the above in a smart way we can make sure our ETS tables will always live as long as the node is up.

#The method

We need a supervised Table Manager gen_server. When it is launched first time, it does practically nothing but starting to wait for incoming requests that could be creating a local ETS table. It's job to check if the table exists and if so, error will be returned. If the table has not been created yet, the Table Manager creates it with the heir option pointing back to itself, then launches a supervised Table Handler gen_server for this new Table and use ets:give_away to give the table ownership to the handler. From now on, all processes can write or delete into the new table through this handler by it's registered name, and read data even direclty with the table id. When all this succeeded, the Table Manager saves the new table's name and Id into a local dets file.

From etsserver.erl
```Erlang
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

```
#When Table Manager dies.

When the Table Manager accidentaly crashes, the supervisor restarts it with a new Pid. The Table Handlers have the old Pid as to give the ownership back if they die, so when the Table Manager restarts, it opens the local dets table, goes through the living ETS tables and lets all the Table Handler processes know that the heir Pid changed. All Table Handlers just make a call to change the heir for their table and the show goes on.

From etshandler.erl:
```Erlang
handle_call({reheir,TableName,TableId,HeirPid}, _From, State) ->
	ets:setopts(TableId, {heir,HeirPid,TableName}),
	{reply,State,State}
;
```

Will be continued......
