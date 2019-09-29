-module(world_svr).
-behaviour(gen_server).
-export([start_link/1, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-record(state, {key,lastontime=0}).

start_link(Key) ->
	?log("**************Info:start world server,key=~p", [Key]),
	process_flag(trap_exit, true),
	{ok, Pid} = gen_server:start_link({global, Key}, ?MODULE, [Key], []),
	?log("key=~p,Pid=~p",[Key,Pid]),
	{ok, Pid}.

stop(Key) ->
	gen_server:cast({global, Key}, stop).

init([Key]) ->  
	Ret = do_init(Key),
	timer:apply_after(1000, gen_server, cast, [self(), {timercast}]),	
    Ret.

handle_call(Request, _From, State) ->   
	Service=State#state.key,
	ServiceImpl=list_to_atom("fun_"++atom_to_list(Service)),
	try 
	   ServiceImpl:do_call(Request,State)
	catch 
		E:R:Stacktrace -> ?log_error("world do_call  error  Service=~p, E=~p,R=~p,stack=~p",[Service,E,R,Stacktrace]),
			   {noreply, ok, State}
    end.
  

handle_cast(stop, State) ->
    {stop, normal, State}; 
handle_cast({timercast}, State) -> 
	Now = util:longunixtime(),    %%ms
	NewState =	case Now - State#state.lastontime >= 1 of
		?TRUE ->
				ontime(Now,State),
				State#state{lastontime=Now};
		?FALSE -> State
	end,
	timer:apply_after(1000, gen_server, cast, [self(), {timercast}]),
	{noreply, NewState};
handle_cast({check}, State) ->
	gen_server:cast({global, sm}, {checkok, atom_to_list(State#state.key)}),
	{noreply, State};
handle_cast(Msg, State) ->
	try 
	    do_msg(Msg, State)
	catch 
		E:R:Stacktrace -> ?log_error("world handle_cast error E=~p,R=~p,Stacktrace=~p",[E,R,Stacktrace])
	end,		   
	{noreply, State}.
handle_info({http, Msg}, State) ->
	fun_http:async_http_response(Msg),
	{noreply, State};
handle_info(Info, State) ->
	try 
	   do_info(Info, State)
    catch 
	   E:R:Stacktrace -> ?log_error("world handle_info  error E=~p,R=~p,Stacktrace=~p",[E,R,Stacktrace])
    end,
	   {noreply, State}.

terminate(_Reason, State) ->
	?log("Info:public server stoped!Reason=~p,State=~p", [_Reason, State]),
	try 
	   do_close(State#state.key)
	catch 
	   E:R:Stacktrace -> ?log_error("world terminate  error E=~p,R=~p,Stacktrace=~p",[E,R,Stacktrace])
    end,	  
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%doinit

do_init(Service) ->
	{ok, SvrNum} = application:get_env(svr_num),
	{ok, Ip} = application:get_env(ip),
	{ok, Port} = application:get_env(port),
	{ok, Intranet} = application:get_env(intranet),
	put(svr_num,SvrNum),
	put(svr_ip,Ip),
    put(svr_port,Port),
	put(intranet,Intranet),
	%% ?log("-------{Service,SvrNum,Ip,Port,RedisPath,RedisPwd,RedisPort}=~p",[{Service,SvrNum,Ip,Port,RedisPath,RedisPwd,RedisPort}]),
	ServiceImpl=list_to_atom("fun_"++atom_to_list(Service)),
	try 
	   ServiceImpl:do_init()
	catch 
		E:R:Stacktrace -> ?log_error("world do_init  error  Service=~p, E=~p,R=~p,stack=~p",[Service,E,R,Stacktrace])
    end,	  
	{ok, #state{key=Service,lastontime=util:longunixtime()}}.
%%dotime
ontime(Now,State) -> do_time(State#state.key, Now).
do_time(Service, Now) ->	
	ServiceImpl=list_to_atom("fun_"++atom_to_list(Service)),
	try 
		ServiceImpl:do_time(Now)
	catch 
		E:R:Stacktrace -> ?log_error("world do_time  error Service=~p ,E=~p,R=~p ,stack=~p",[Service,E,R,Stacktrace])
    end.




%%domsg
do_msg(Msg, #state{key = Service}) ->
	ServiceImpl=list_to_atom("fun_"++atom_to_list(Service)),
	try 
		ServiceImpl:do_msg(Msg)
	catch 
		E:R:Stacktrace -> ?log_error("world do_msg  error Service=~p ,E=~p,R=~p,stack=~p",[Service,E,R,Stacktrace])
    end.


%%doinfo

do_info(Info, #state{key = Service}) ->
	ServiceImpl=list_to_atom("fun_"++atom_to_list(Service)),
	try 
		ServiceImpl:do_info(Info)
	catch 
		E:R:Stacktrace -> ?log_error("world do_info  error Service=~p , E=~p,R=~p,stack=~p",[Service,E,R,Stacktrace])
    end.

%%doclose
do_close(Service) ->
	ServiceImpl=list_to_atom("fun_"++atom_to_list(Service)),
	try 
		ServiceImpl:do_close()
	catch 
		E:R:Stacktrace -> ?log_error("world do_close  error  Service=~p , E=~p,R=~p,Stacktrace=~p",[Service,E,R,Stacktrace])
    end.