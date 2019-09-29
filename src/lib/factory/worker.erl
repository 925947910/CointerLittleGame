-module(worker).
-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3,stop/1]).
-export([behaviour_info/1]).

-include("td.hrl").
-include("log.hrl").

-record(state, {idx = 0,config={},workInfo = {},work={}}).

behaviour_info(callbacks) ->
    [{do_init,2},{do_work,2},{get_task,1},{do_init_factory,1},{do_terminate,2},{do_info_factory,2},{do_call_factory,3}];
behaviour_info(_Other) ->
    undefined.

%%{Idx,{WorkName,WorkModule,WorkArg}}
start_link({Idx,{WorkName,_,_} } = WorkConfig) ->
	Me = factory:get_worker_name(WorkName,Idx),
	gen_server:start_link({local, Me}, ?MODULE, WorkConfig, []).
	
stop(Name) ->
    gen_server:cast({global, Name}, stop).

init({Idx,{WorkName,WorkModule,WorkArg} = WorkConfig}) ->
	try
		case WorkModule:do_init(Idx,WorkArg) of
			{ok,Info} -> cast_fac(WorkName,{new, Idx}),
						 {ok, #state{idx = Idx,config=WorkConfig,workInfo = Info}};
			Re -> {stop, {initfail,Re}}
		end
	catch E:R -> {stop, {callinitfail,E,R}}
	end.

terminate(Reason,#state{idx = Idx ,config = {WorkName,_,_}}) ->
	cast_fac(WorkName,{terminate, Reason, Idx}),
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call(where, _From, State) ->
    {reply, {ok,self()}, State};

handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_info(_Request, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({run,Work},#state{idx = Idx ,workInfo = Info,config = {WorkName,WorkModule,_}} = State) ->
%% 	?debug("WorkName=~p,Idx=~p,Work=~p",[WorkName,Idx,Work]),
  	try
		case WorkModule:do_work(Work,Info) of
			{ok,NewInfo} -> cast_fac(WorkName, {free, Idx}),
							{noreply, State#state{workInfo = NewInfo}};
			Re -> {stop, {castfail,Work,Re}, State}
		end
	catch E:R -> {stop,{callcastfailWork,Work,E,R},State}
	end;

handle_cast(_Re, State) ->
	{noreply, State}.

cast_fac(WorkName,Msg) ->
	Factory = factory:get_factory_name(WorkName),
	gen_server:cast(Factory, Msg).