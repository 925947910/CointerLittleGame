 -module(fun_redisConn).
-include("common.hrl").

-export([
		 do_call/2,
		 do_init/0,
		 do_msg/1,
		 do_info/1,
		 do_time/1,
		 do_close/0,
		 send_msg/1
		]).
-define(CleanCd,60).

do_init() ->
	init_redis_conn().

init_redis_conn()->
	{ok, RedisPath} = application:get_env(redis_path),
	{ok, RedisPwd} = application:get_env(redis_pwd),
	{ok, RedisPort} = case application:get_env(redis_port) of  
						  {ok,_RedisPort}->{ok, _RedisPort};
						  ?UNDEFINED->{ok, 6379}
					  end,
	put(redis_path,RedisPath),
	put(redis_pwd,RedisPwd),
    put(redis_port,RedisPort),
	put(redisConns,{[],[]}).
do_time(Time)->
%% 	Clock=(Time div 1000)rem 10,
%% 	if Clock==1->ok;
%% 	   true->skip
%% 	end,
	clean_conns().	



do_call({get_redis,DB},State) ->
	Redis=get_conn(DB),
	{reply, Redis, State};

do_call(_,State) ->	
	{reply, nomatch, State}.



do_msg({onlines,Onlines})->
	SvrNum=get(svr_num),
	SvrIp=get(svr_ip),
	Intranet=get(intranet),
	SvrPort=get(svr_port),
	case get_conn(?STATIC_DATA_DB) of
		Pid  when erlang:is_pid(Pid)->
			try
				Args=[["HMSET", SvrNum,"ip", SvrIp, "port", SvrPort, "conns", Onlines,"intranet",Intranet],["EXPIRE",SvrNum,60]],
			    eredis:qp(Pid,Args),
			    return_conn(?STATIC_DATA_DB,Pid)
			catch
				E:R:Stacktrace -> ?log_error("eredis:q error E=~p,R=~p,stack=~p",[E,R,Stacktrace]),
					return_conn(?STATIC_DATA_DB,Pid)
			end;
		_->skip
	end;


do_msg({return_conn,DB,Pid})->
	return_conn(DB,Pid);

do_msg(Msg)-> ?log_warning("fun_redisConn msg Unmatch Msg:~p,~p",[Msg,erlang:get_stacktrace()]).


do_info({timeout, _TimerRef, CallBackInfo}) ->
	%% 	erlang:start_timer(Time, Dest, Msg),
	case CallBackInfo of
		{Module, Function} ->
			apply(Module, Function, []);
		{Module, Function, Args} ->
			apply(Module, Function, Args);
		_ ->
			?log_warning("unknown timer callback,CallbackInfo=~p", [CallBackInfo])
	end;

do_info(Info)-> ?log("fun_redisConn info Info:~p",[Info]).

do_close() -> ?log("fun_redisConn closed").

%%other use
send_msg(Msg) ->?send_world(redisConn,Msg).

new_conn(DB)->
	Ip=get(redis_path),
	Pwd=get(redis_pwd),
	Port=get(redis_port), 
	case  eredis:start_link(Ip,Port,DB,Pwd) of   
		{ok, ClientPid}->
			RedisConnData=get(redisConns),
%% 			?log_warning("NEW_CONN {free,used}~p",[RedisConnData]),
			ClientPid;
		_R->?log_error("create_redis_conn_failed~p",[_R]),failed
	end.
select_db([],_,Remain)->{?FALSE,Remain};
select_db([{DB,Pid}|D],DB,Remain)->{Pid,Remain++D};
select_db([H|D],DB,Remain)->select_db(D,DB,[H|Remain]).

get_conn(DB)->
	case get(redisConns)  of  
		{FreeConns,Conns}->
			case select_db(FreeConns,DB,[])  of  
				{ClientPid,Remain} when erlang:is_pid(ClientPid)->
					case eredis:q(ClientPid, ["PING"])  of 
						{ok,<<"PONG">>}->
							put(redisConns,{Remain,[{DB,ClientPid}|Conns]}),
							ClientPid;
						_-> eredis:stop(ClientPid),
							put(redisConns,{Remain,Conns}),
							get_conn(DB)
					end;
				_->
					case new_conn(DB) of  
						ClientPid when erlang:is_pid(ClientPid)->
							put(redisConns,{FreeConns,[{DB,ClientPid}|Conns]}),
							ClientPid;
						_->failed
					end
			end;
		_->failed
	end.

return_conn(DB,Pid)->
	case get(redisConns)  of  
		{Frees,Conns}->
			case lists:member({DB,Pid}, Conns) of  
				true->	put(redisConns,{[{DB,Pid}|Frees],lists:delete({DB,Pid},Conns)}),
						ok;
				_->eredis:stop(Pid)
			end;
		_->skip
	end.


add_connect(DB,Host, Port, Usr, Psw, Database,Code) ->
	?log("dbm connect db,DB=~p,Host=~p,Port=~p,Usr=~p,Database=~p,Code=~p", [DB,Host,Port,Usr,Database,Code]),
	case mysql:connect(DB, Host, Port, Usr, Psw, Database,Code, true) of
		{?OK, _} -> ?OK;
		Other1 -> ?log_error("Error:dbm init,mysql:connect.Other=~p", [Other1])
	end.
clean_conns()->
	case  get(cleanCD) of  
		undefined->put(cleanCD,?CleanCd);
		0->
			put(cleanCD,?CleanCd),
			case get(redisConns)  of  
				{Frees,Conns} when length(Frees)>length(Conns)->
					Point=length(Frees) div 2,
					{L1,L2}=lists:split(Point, Frees),
					Fun=fun({_,Pid})->  
								eredis:stop(Pid)		
						end,
					lists:foreach(Fun,L1),
					put(redisConns,{L2,Conns});
				_->skip
			end;
		Num when erlang:is_number(Num)->
			put(cleanCD,Num-1)
	end.


