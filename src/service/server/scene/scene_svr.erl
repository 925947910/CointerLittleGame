-module(scene_svr).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3]).

-include("common.hrl").

-record(state, {id=0,hid=0,type=0,start_time=0,check_time=0,closeReason=''}).
-define(STM_N,100).
start_link(Data) ->
    gen_server:start_link(?MODULE, Data, []).

stop() ->
    gen_server:cast(?MODULE, {stop,kill}).
     


init({SceneId,GameId,Opt,UserData,RecCode}) ->
	try   
		#static_game{mod=Mod}=static_games:get_data(GameId),
		put(run_mod,Mod),
		fun_scene:on_init({SceneId,GameId,Opt,UserData,RecCode})
	catch 
		E:R:StackTrace -> ?log_error("scene init error E=~p,R=~p,stack=~p",[E,R,StackTrace])
	end,
	Now=util:longunixtime(),
	fun_sceneMng:send_msg({scene_add,SceneId,self()}),
	timer:apply_after(?SCENE_TIMER_INTERVAL, gen_server, cast, [self(), {time}]),
	{ok, #state{id=SceneId,hid=self(),start_time=Now,check_time=Now+?SCENE_TIMER_INTERVAL}}.	
    

terminate(_Reason, #state{id=Id,closeReason=CloseReason}) ->
	?log_warning("terminate======================ID~p",[{Id,CloseReason}]),
%% 	Agent=fun_scene:get_agent(),
    fun_scene:on_close(Id,CloseReason).

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call(Request, _From, State) ->  
	try 
	   fun_scene:do_call(Request,State)
	catch 
		E:R:Stacktrace -> ?log_error("scene do_call  error   E=~p,R=~p,stack=~p",[E,R,Stacktrace]),
		{noreply, ok, State}
    end.


handle_info({http, Msg}, State) ->
	fun_http:async_http_response(Msg),
	{noreply, State};

handle_info({timeout, _TimerRef, CallBackInfo}, State) ->
	try
		case CallBackInfo of
			{Module, Function} ->
				apply(Module, Function, []);
			{Module, Function, Args} ->
				apply(Module, Function, Args);
			_ ->
				?log_warning("unknown timer callback,CallbackInfo=~p", [CallBackInfo])
		end
	catch
		ES:RS:Stacktrace -> 
			?log_error("scene handle_info error {timeout, _TimerRef, CallBackInfo=~p},E=~p,R=~p,c=~p",[CallBackInfo,ES,RS,Stacktrace])
	end,
	{noreply, State};

handle_info({stop,Reason}, State) ->
	 {stop, normal, State#state{closeReason=Reason}};

handle_info(_Request, State) ->
    {noreply, State}.



handle_cast({time},  #state{check_time=CheckTime} = State) -> 
	Now = util:longunixtime(),
	Delay=if Now>(CheckTime+?STM_N)->1;
			 true->CheckTime+?STM_N-Now
		  end,
	timer:apply_after(Delay, gen_server, cast, [self(), {time}]),
	try
		fun_scene:do_time(Now)
	catch
		ES:RS:Stacktrace -> 
			%% 			?log("scene doTimert error SceneModel=~p,E=~p,R=~p",[SceneModel,ES,RS])
			?log_error("scene doTimert error SceneModel=~p,E=~p,R=~p,c=~p",[1,ES,RS,Stacktrace])
			
	end,
	{noreply, State#state{check_time=CheckTime+?STM_N}};
handle_cast({msg,Msg}, #state{id=_Room,type =ModuleType} = State) ->
%% 	?log("msg:Type = ~p,Msg = ~p ",[Scene,Msg]),
	try
		fun_scene:do_msg(Msg)
	catch
		E:R:Stacktrace -> 
            ?log_error("scene handle_cast script error Msg=~p,Scene=~p,E=~p,R=~p,stacktrace=~p",[Msg,ModuleType,E,R,Stacktrace])
	end,
    {noreply, State};

handle_cast({stop,Reason}, State) ->
           {stop, normal, State#state{closeReason=Reason}};
	
handle_cast(_Request, State) ->
	?log("Debug:scene receive unknow cast,id=~p,msg=~p", [State#state.hid, _Request]),
    {noreply, State}.






