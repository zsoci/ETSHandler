ETSHandler
===========
Example for application that makes sure you do not lose your ETS tables.

Steve Vinoski wrote an excellent article about the subject.
http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/
http://steve.vinoski.net/blog/2013/05/08/implementation-of-dont-lose-your-ets-tables/

He also states "The Erlang supervisor creates a table manager process. Since all this process does is manage the table, the likelihood of it crashing is very low."
And he is right. Anyway likelihood is low means it can happen.

For experiment and playing around with ETS tables, there is a way to make this likelihood even lower. The code handles basic ETS operations, store, load and delete. Can be further developed as you wish.

#Limitations

This is an example project, does not implement all ets features. The tables are bag type with protected access. If you need to expand the capabilities, like passing ets options when creating a new table or adding more ets functions, you are free to contribute.

#The idea

The goal is not to lose ETS table as long as the node is alive. Having a process owning the table is a scenario where we can lose the table when that process dies for any reason, even if a supervisor restarts it, ETS is gone.

There is a way to pass ETS table ownership when a process dies. The option heir and the function ets:give_away/3 do the job and are documented on http://www.erlang.org/doc/man/ets.html#give_away-3

Using the above in a smart way we can make sure our ETS tables will always live as long as the node is up.

#Definitions

* Table Manager is a gen_server. It's job to create new ETS table, it's handler process and to act as an owner to get the ETS ownership back when a Table Handler crashes.
* Table Handler is a gen_server. It is responsible to take ownership for a newly created ETS table and perform ETS table operations.

#The method

We need a supervised Table Manager gen_server. When it is launched first time, it does practically nothing but starting to wait for incoming requests that could be creating a local ETS table. It's job to check if the table exists and if so, error will be returned. If the table has not been created yet, the Table Manager creates it with the heir option pointing back to itself, then launches a supervised Table Handler gen_server for this new Table and use ets:give_away to give the table ownership to the handler. From now on, all processes can write or delete into the new table through this handler by it's registered name, and read data even direclty with the table id. When all this succeeded, the Table Manager saves the new table's name and Id into a local dets file.

There is a situation we need to handle additionally. When the Table Manager creates the Table Handler, the Table Handler has not got the ownership yet, so in case some process wants to write to the new table through the Handler it might fail. To solve this, Table Hanldler retries the write with delayed_store, delayed_load and delayed_delete as long as it's does not have the table ownership.

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

#When Table Handler dies

In this case as we have set up heir, Table Manager gets an {'ETS-TRANSFER',TableId,FromPid,TableName} message, simply waits for the new Handler to be restarted by the supervisor and give the ownership back to the new Handler.

From etssserver.erl:
```Erlang
handle_info({'ETS-TRANSFER',TableId,FromPid,TableName}, State) ->
	give_away(TableName,TableId),
    {noreply, State}
;
....
....
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
```

#Think beyond the limits

The above seems to be quite fault tolerant, however, what happens if a process wants to store a value through etshandler and at the same time the handler crashes. The initiating process gets an exception as the gen_server is not found (registered). To overcome even on this wery unlikely situation, there is a wrapper for calling a gen_server that handles the challenge by protecting the gen_server:call with a try catch block and tries to perform the call for a given times with a given wait value by retry. Hopefully the crashed server will be restarted by the supervisor in the timeframe this wrapper tries to call it repeatedly, so for the initial caller, the whole process is hidden.

From utils.erl:
```Erlang
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
```

#When everything goes wrong
Well, althought the above architecture seems highly available, there are situations when it crashes. Like on an overloaded server a crashed handler or table manager cannot be restarted. In this case, we need to tune the wrapper defaults or consider the situation as a node down event, as if a process cannot reply for a long time, the functionality of that node is not there anyway.

#Let us see if it works

To have the supervisors, I put the whole solution into an application called multiserver.

When you launch:```test:all().``` It will start the application and performs the following in etsserver.hrl:

```
test() ->
	newtable(ets_test1),
	newtable(ets_test2),
	die(),
	timer:sleep(2000),
	etshandler:die(ets_test1),
	timer:sleep(2000),
	showtables()
.
```

The result in erl shell shall be the following:
```
multiserver:Got a startup message.
etsserver:Etsserver init
etshandler:Got the table ownership for table:ets_test1. It was:undefined
etshandler:Got the table ownership for table:ets_test2. It was:undefined
etsserver:etsserver terminated for reason:{badarg,
                                           [{etsserver,handle_cast,2,
                                             [{file,
                                               "d:/Develop/Luna/ETSHandler/src/etsserver.erl"},
                                              {line,175}]},
                                            {gen_server,handle_msg,5,
                                             [{file,"gen_server.erl"},
                                              {line,604}]},
                                            {proc_lib,init_p_do_apply,3,
                                             [{file,"proc_lib.erl"},
                                              {line,239}]}]}
etsserver:Etsserver init
etshandler:ets_test1 got a reheir request to <0.63.0> for table 24597

etshandler:ets_test2 got a reheir request to <0.63.0> for table 28694


=ERROR REPORT==== 19-Nov-2014::17:09:33 ===
** Generic server etsserver terminating 
** Last message in was {'$gen_cast',die}
** When Server state == undefined
** Reason for termination == 
** {badarg,[{etsserver,handle_cast,2,
                       [{file,"d:/Develop/Luna/ETSHandler/src/etsserver.erl"},
                        {line,175}]},
            {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,604}]},
            {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
etshandler:Got a die request

etshandler:etshandler terminated for reason:{badarg,
                                             [{etshandler,handle_cast,2,
                                               [{file,
                                                 "d:/Develop/Luna/ETSHandler/src/etshandler.erl"},
                                                {line,136}]},
                                              {gen_server,handle_msg,5,
                                               [{file,"gen_server.erl"},
                                                {line,604}]},
                                              {proc_lib,init_p_do_apply,3,
                                               [{file,"proc_lib.erl"},
                                                {line,239}]}]}


=ERROR REPORT==== 19-Nov-2014::17:09:35 ===
** Generic server ets_test1 terminating 
** Last message in was {'$gen_cast',die}
** When Server state == 24597
** Reason for termination == 
** {badarg,[{etshandler,handle_cast,2,
                        [{file,"d:/Develop/Luna/ETSHandler/src/etshandler.erl"},
                         {line,136}]},
            {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,604}]},
            {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
etsserver:Got the table ownership back for table:24597 from <0.57.0>. Table name is :ets_test1
etshandler:Got the table ownership for table:ets_test1. It was:undefined
etsserver:The following ets tables exist:

etsserver:Table:ets_test1,Id:24597

etsserver:Table:ets_test2,Id:28694

etsserver:Got the table ownership back for table:28694 from <0.61.0>. Table name is :ets_test2
multiserver:multiserver terminated for reason:shutdown

=INFO REPORT==== 19-Nov-2014::17:09:37 ===
    application: multiserver
    exited: stopped
    type: temporary
ok
```

If you analyze the output, you will find out, the code works as planned.

===========
Improvements and feedbacks are welcome.

#Acknowledgements
Thanks to Steve Vinoski for the base idea and comments on the limitations.

Thanks to Csaba Hoch for the initial idea to use ets tables for dynamically generated process names instead of atoms.

