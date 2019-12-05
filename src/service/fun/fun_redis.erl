%% @author User
%% @doc @todo Add description to fun_redis.


-module(fun_redis).
-include("common.hrl").
-export([get_conn/1,return_conn/2]).
-export([lock/2,unlock/2,dirty_qp/1,select_dirty_qp/2]).
-export([add_event/2,in_game/3,gen_session/1,gen_item/2]).

-define(RedisTimeOut,5000).
-define(RedisLockSecound,10).
-define(RedisRetryTimes,30).

kvs([],Result)->Result;
kvs([K|[V|Remain]],Result)->
	kvs(Remain,[{K,V}|Result]).
equals(A,B)->util:to_list(A)==util:to_list(B).

get_conn(DB)->
	gen_server:call({global,redisConn}, {get_redis,DB}).
return_conn(DB,Pid)->
	fun_redisConn:send_msg({return_conn,DB,Pid}).

dirty_qp(Args)->
	dirty_qp(Args, ?RedisRetryTimes).

select_dirty_qp(Args,DB)->
	select_dirty_qp(Args, ?RedisRetryTimes,DB).

dirty_qp(Args,0)->?log_error("dirty_qp_timeout======Args:~p",[Args]),failed;
dirty_qp([Args],Times)->
	case get_conn(0) of
		Pid  when erlang:is_pid(Pid)->
			try
				Result=eredis:q(Pid, Args),
				return_conn(0,Pid),
				[Result]
			catch
				E:R:Stacktrace -> ?log_error("eredis:q error E=~p,R=~p,Args~p,stack=~p",[E,R,Args,Stacktrace]),
					return_conn(0,Pid),failed
			end;
		_->
			timer:sleep(100),
			dirty_qp([Args],Times-1)
	end;
dirty_qp(Args,Times)->
	case get_conn(0) of
		Pid  when erlang:is_pid(Pid)->
			try
				Result=eredis:qp(Pid, Args),
				return_conn(0,Pid),
				Result
			catch
				E:R:Stacktrace -> ?log_error("eredis:qp error E=~p,R=~p,Args~p,stack=~p",[E,R,Args,Stacktrace]),
					return_conn(0,Pid),failed
			end;
		_->
			timer:sleep(100),
			dirty_qp(Args,Times-1)
	end.
select_dirty_qp(Args,0,DB)->?log_error("select_dirty_qp_get_conn_timeout======DB:~p Args:~p",[DB,Args]),failed;
select_dirty_qp([Args],Times,DB)->
	case get_conn(DB) of
		Pid  when erlang:is_pid(Pid)->
			try
				Result=eredis:q(Pid, Args),
				return_conn(DB,Pid),
				[Result]
			catch
				E:R:Stacktrace -> ?log_error("eredis:qp error E=~p,R=~p,Args~p,stack=~p",[E,R,Args,Stacktrace]),
					return_conn(DB,Pid),failed
			end;
		_->
			timer:sleep(100),
			select_dirty_qp([Args],Times-1,DB)
	end;
select_dirty_qp(Args,Times,DB)->
	case get_conn(DB) of
		Pid  when erlang:is_pid(Pid)->
			try
				Result=eredis:qp(Pid, Args),
				return_conn(DB,Pid),
				Result
			catch
				E:R:Stacktrace -> ?log_error("eredis:qp error E=~p,R=~p,Args~p,stack=~p",[E,R,Args,Stacktrace]),
					return_conn(DB,Pid),failed
			end;
		_->
			timer:sleep(100),
			select_dirty_qp(Args,Times-1,DB)
	end.

lock(Key,DB)->
  case  select_dirty_qp([["SET",util:to_list(Key)++"_lock-with-expire-and-NX",?Btrue,"EX",?RedisLockSecound,"NX"]],DB) of
    [{ok,<<"1">>}]->ok;
    _->timer:sleep(100),
      lock(Key,DB)
  end.
unlock(Key,DB)->select_dirty_qp([["DEL",util:to_list(Key)++"_lock"]],DB).

%%   Event={obj,[{"uid",Uid},{"E",?EVENT_WIN},{"price",Price},{"game",GameId},{"desc","play_game"}]},
%%   Event={obj,[{"uid",Uid},{"E",?EVENT_PAY},{"pay",Coin},{"game","GameId"},{"desc","play_game"}]},
%%   Event={obj,[{"uid",Uid},{"E",?EVENT_PAY},{"pay",Coin},{"game","GameId"},{"desc","buy_skin"}]},
add_event(Id,Events) when Events=/=[]->
	Distributor="Distributor:"++util:to_list(Id rem 10),
	Queue="Event:"++util:to_list(Id),
	Fun=fun(Obj)->
				Data=rfc4627:encode(Obj),
				["RPUSH",Queue,Data]
		end,
	Args=lists:map(Fun, Events),
	Event=[["RPUSH",Distributor,Id]]++Args,
	fun_redis:select_dirty_qp(Event,?EVENT_DB).

in_game(Uid,GameId,InOrOut)->
	Key="inGame:"++util:to_list(Uid),
	case InOrOut of  
		 in->fun_redis:select_dirty_qp([["HSET",Key,game,GameId],["EXPIRE",Key,3600]],?USER_STATUS_DB);
		 _-> fun_redis:select_dirty_qp([["DEL",Key]],?USER_STATUS_DB)
	end.
	
gen_session(Uid)->"user:"++util:to_list(Uid).
gen_item(ItemKey,Uid)->ItemKey++util:to_list(Uid).





