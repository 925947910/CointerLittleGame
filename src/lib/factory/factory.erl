-module(factory).
-behaviour(gen_server).

-export([start_link/1,  init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3,get_factory_name/1,get_worker_name/2]).

-include("db.hrl").
-include("log.hrl").

%%mode = fast,done
-record(state, {name="",config={},tasklist=[],workerlist=[],num=0,facInfo = {},mode=done}).

get_factory_name(WorkName) -> 
	list_to_atom(WorkName ++ "_factory").

get_worker_name(WorkName,Idx) -> 
	list_to_atom(WorkName ++ "_work_" ++ util:to_list(Idx)).

%% {{WorkName,WorkModule,WorkArg},Num}}
start_link({{WorkName,_,_},_,_} = Config) ->
	Me = get_factory_name(WorkName),
    gen_server:start_link({local, Me}, ?MODULE, Config, []).

init({Config = {WorkName,WorkModule,WorkArg},Num,Mode}) ->
	try
		case WorkModule:do_init_factory(WorkArg) of
			{ok,Info} -> 
				F = fun(I) -> worker_sup:add(WorkName,{I,Config}) end,
				if 
					Num > 1 -> util:for(1, Num, F);
					true -> ok
				end,
				{ok, #state{name=WorkName,config = Config,workerlist = [],facInfo=Info,num=Num,mode=Mode}};
			Re -> {stop, {initfail,Re}}
		end
	catch E:R -> {stop, {callinitfail,E,R,erlang:get_stacktrace()}}
	end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call(Request,From, #state{config = {_,WorkModule,_},facInfo = Info} =State) ->
	try
		case WorkModule:do_call_factory(Request,From,Info) of
			{ok,End,NewInfo} -> {reply,End, State#state{facInfo = NewInfo}};
			R -> ?log_error("do_call_factory error Request=~p Info=~p R =~p",[Request,Info,R]),{reply,fail, State}
		end
	catch E:Re -> ?log_error("do_call_factory error Request=~p Info=~p E=~p Re =~p",[Request,Info,E,Re]),{reply,fail, State}
	end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({checktask},#state{workerlist = Workerlist,config = {WorkName,WorkModule,_},facInfo = Info,mode=done} =State) ->
	{ok,NewTaskList1,NewInfo} = check_task(WorkModule,Info),
	case NewTaskList1 of
		[] -> erlang:send_after(?WORK_TIME, self(), {checktask}),
			  {noreply, State#state{facInfo = NewInfo}};
		_ -> 
%% 			?debug("get task WorkName=~p,NewTaskList = ~p",[WorkName,NewTaskList1]),
			{NewTaskList,NewWorkerlist} = alt_all(WorkName,NewTaskList1,Workerlist),
			{noreply, State#state{facInfo = NewInfo,tasklist=NewTaskList,workerlist = NewWorkerlist}}
	end;

handle_info(Request, #state{config = {_,WorkModule,_},facInfo = Info,mode=done} =State) ->
	try
		case WorkModule:do_info_factory(Request,Info) of
			{ok,_,NewInfo} -> {noreply, State#state{facInfo = NewInfo}};
			R -> ?log_error("do_info_factory1 error Request=~p Info=~p R =~p",[Request,Info,R]),{noreply, State}
		end
	catch E:Re -> ?log_error("do_info_factory2 error Request=~p Info=~p E=~p Re =~p",[Request,Info,E,Re]),{noreply, State}
	end;

handle_info(Request, #state{config = {WorkName,WorkModule,_},facInfo = Info,mode=fast} =State) ->
	try
		case WorkModule:do_info_factory(Request,Info) of
			{ok,free,NewInfo} -> {noreply, State#state{facInfo = NewInfo}};
			{ok,{Idx,Task},NewInfo} -> cast_worker(WorkName,Idx,{run,Task}),
									   {noreply, State#state{facInfo = NewInfo}};
			R -> ?log_error("do_info_factory3 error Request=~p Info=~p R =~p",[Request,Info,R]),{noreply, State}
		end
	catch E:Re -> ?log_error("do_info_factory4 error Request=~p Info=~p E=~p Re =~p",[Request,Info,E,Re]),{noreply, State}
	end.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({terminate,Reason,  Idx},  #state{config = {_,WorkModule,_}} =State) ->
	try
		WorkModule:do_terminate(Reason,Idx)
	catch E:R -> ?log_error("do_terminate error E =~p, r = ~p",[E,R])
	end,
	{noreply, State};

handle_cast({new,  Idx},  #state{tasklist=Tasklist,workerlist = Workerlist,config = {WorkName,_,_},num=Num,mode=done} =State) ->
	?debug("worker init WorkName=~p Idx=~p",[WorkName,Idx]),
	{NewTaskList,NewWorkerlist} = alt_task(WorkName,Tasklist,Workerlist,Idx,Num),
	{noreply, State#state{tasklist=NewTaskList,workerlist = NewWorkerlist}};

handle_cast({free,  Idx},  #state{tasklist=Tasklist,workerlist = Workerlist,config = {WorkName,_,_},num=Num,mode=done} =State) ->
	?debug("worker free WorkName=~p Idx=~p",[WorkName,Idx]),
	{NewTaskList,NewWorkerlist} = alt_task(WorkName,Tasklist,Workerlist,Idx,Num),
	{noreply, State#state{tasklist=NewTaskList,workerlist = NewWorkerlist}};

handle_cast({new,  Idx},  #state{workerlist = Workerlist,config = {WorkName,_,_},num=Num,mode=fast} =State) ->
	?debug("worker init WorkName=~p Idx=~p",[WorkName,Idx]),
	Ffind = fun(I) -> if 
						  I == Idx -> false;
						  true -> true
					  end end,
	Workerlist1 = lists:filter(Ffind, Workerlist),
	NewWorkerlist = lists:append(Workerlist1,[Idx]),
	WorkerLen = erlang:length(NewWorkerlist),
	if
		WorkerLen >= Num -> ?log("all worker started now ~p",[WorkName]);
		true -> ok
	end,
	{noreply, State#state{workerlist = NewWorkerlist}};

handle_cast({free,  _Idx},  #state{mode=fast} =State) ->
	{noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

alt_all(_WorkName,[],Workerlist) -> {[],Workerlist};
alt_all(_WorkName,TaskList,[]) -> {TaskList,[]};
alt_all(WorkName,[Task|NextTaskList],[Idx|NextWorkerlist]) ->
	Ffind = fun(I) -> if 
						  I == Idx -> false;
						  true -> true
					  end end,
	
	cast_worker(WorkName,Idx,{run,Task}),
	NewWorkerlist = lists:filter(Ffind, NextWorkerlist),
	alt_all(WorkName,NextTaskList,NewWorkerlist).


alt_task(WorkName,Tasklist,Workerlist,Idx,Num)->
	Ffind = fun(I) -> if 
						  I == Idx -> false;
						  true -> true
					  end end,
	
	case Tasklist of
		[] -> NewWorkerlist = lists:filter(Ffind, Workerlist),
			  WorkerLen = erlang:length(NewWorkerlist) + 1,
%% 			  ?log("WorkerLen=~p",[WorkerLen]),
			  if
				  WorkerLen >= Num -> ?log("task finish ,flush task now ~p",[WorkName]),erlang:send_after(?WORK_TIME, self(), {checktask});
				  true -> ok
			  end,
			  
			  {Tasklist,lists:append([Idx],NewWorkerlist)};
		[Task|NewTasklist] -> 
			NewWorkerlist = lists:filter(Ffind, Workerlist),
			cast_worker(WorkName,Idx,{run,Task}),
			{NewTasklist,NewWorkerlist};
		_ -> ?log_error("Tasklist error Tasklist = ~p",[Tasklist])
	end.

check_task(WorkModule,Info) ->
	try
		case WorkModule:get_task(Info) of
			{ok,GetTaskList,NewInfo} -> {ok,GetTaskList,NewInfo};
			R -> ?log_error("check_task error R =~p",[R]),{ok,[],Info}
		end
	catch E:Re -> ?log_error("check_task error E=~p Re =~p",[E,Re]),{ok,[],Info}
	end.

cast_worker(Name,Indx,Msg) ->
%% 	?log("cast_worker Name=~p,Indx=~p,Msg=~p",[Name,Indx,Msg]),
	Worker = get_worker_name(Name,Indx),
	gen_server:cast(Worker, Msg).