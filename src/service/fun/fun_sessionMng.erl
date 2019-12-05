-module(fun_sessionMng).
-include("common.hrl").

-define(UPDATEOLCD,30).
-define(ConnStatus,'ConnStatus').
-define(ConnOpen,1).
-define(ConnClose,0).
-export([
	do_init/0,
	do_call/2,
	do_msg/1,
	do_info/1,
	do_time/1,
	do_close/0,
	send_msg/1,
	call/1
]).
%%my fun


do_init() ->
	db:init_ets_table(user_net, 2).


do_time(Now)->
	clear_match(Now div 1000),
	get_onlines().


do_call(_,State) ->
	{reply, nomatch, State}.

do_msg({sceneUdp,Host,Port,Data}) ->
	case Data  of  
		<<_Len:?u32,Uid:?u32,_/binary >>->
			case db:get_ets_data(user_net, Uid)  of  
				[#user_net{scene=Scene,online=?TRUE}]when erlang:is_pid(Scene)->fun_scene:send_msg(Scene,{sceneUdp,Host,Port,Data});
				 _R->skip
			end;
		_->skip
	end;

do_msg({sessionMsg,Uid,Msg}) ->
	case db:get_ets_data(user_net, Uid)  of  
		[#user_net{sid=Sid,online=?TRUE}] ->
			fun_session:session_msg(Sid, Msg);
		_->skip
	end;

do_msg({sceneMsg,Uid,Msg}) ->
	case db:get_ets_data(user_net, Uid)  of  
		[#user_net{scene=Scene,online=?TRUE}]when erlang:is_pid(Scene)->
			fun_scene:send_msg(Scene, Msg);
		_R->skip
	end;
do_msg({in_scene,SceneHid,Players}) ->
	Fun=fun(Uid)->case db:get_ets_data(user_net, Uid)  of  
					  [#user_net{}=OldNet] ->
						  db:put_ets_data(user_net,OldNet#user_net{scene=SceneHid});
					  _R->skip
				  end
		end,
	lists:foreach(Fun, Players);
do_msg({out_scene,SceneHid,Players}) ->
		Fun=fun(Uid)->case db:get_ets_data(user_net, Uid)  of  
					  [#user_net{scene=SceneHid}=OldNet] ->
						  db:put_ets_data(user_net,OldNet#user_net{scene=0});
					  _->skip
				  end
		end,
	lists:foreach(Fun, Players);


do_msg({login_game,{Uid,_Code,Game},Sid}) ->
	NewNet=case db:get_ets_data(user_net, Uid) of
		[#user_net{sid=OldSid,online=Online,scene=Scene}=OldNet]->
			if Online==?TRUE->
				   ?discon(OldSid,'login_repeat',1);
			   true->skip
			end,
			if  is_pid(Scene)->
					fun_scene:send_msg(Scene, {reconn,Uid,Sid});
				true->skip
			end,
			OldNet;
		_->#user_net{id=Uid}
	end,
	case load_user(Uid,Game) of
		#user_net{name=RName,sex=RSex,photo=RPhoto,coin=RCoin,skin=Skin}->
			db:put_ets_data(user_net,NewNet#user_net{name=RName,sex=RSex,photo=RPhoto,coin=RCoin,sid=Sid,game=Game,online=?TRUE}),
			gen_server:cast(Sid,{setConnState, ?USERCONNECTED,Uid}),
            fun_redis:in_game(Uid,Game,in),
			{ok,Bin}=pt_writer:write(?RES_LOGIN,{?Btrue,RName,RSex,RPhoto,RCoin,Skin}),												
			?send(Sid,Bin);
		_->
			{ok,Bin}=pt_writer:write(?RES_LOGIN,{?Bfalse,"",0,"",0,0}),												
			?send(Sid,Bin),
			?discon(Sid,login_failed,500)
	end;

do_msg({match_game,Uid,SelectOpt,Sid})->
	case db:get_ets_data(user_net, Uid) of
		[#user_net{id=Uid,sid=Sid,matching=Matching,game=Game}=OldNet]->
			 Opt=if Matching==0->SelectOpt;
					   true->Matching
					end,
			 Session=fun_redis:gen_session(Uid),
			Coin=case fun_redis:select_dirty_qp([["HGET",Session,coin]],?USER_DB) of  
					 [{ok,BinCoin}] when BinCoin=/=?UNDEFINED ->
						 util:to_integer(BinCoin);
					 _->0
				 end,
			db:put_ets_data(user_net,OldNet#user_net{coin=Coin}),
			match_game(Uid, Game, Opt, Coin, Sid);
		_->skip
	end;
do_msg({quit_match,Uid,Sid})->
	case db:get_ets_data(user_net, Uid) of
		[#user_net{sid=Sid,game=Game,matching=Matching}]when Matching=/=0->
			quit_match(Uid,Game, Matching, Sid);
		_->skip
	end;
do_msg({tcp_close,Conn,Uid,Sid,_Reason}) ->
	case Conn of
		?USERCONNECTED->
			case db:match_ets_data(user_net, #user_net{id=Uid,sid=Sid,_='_'})  of
				[#user_net{scene=Scene,game=Game,matching=Matching}]->
					if Matching=/=0->
						   quit_match(Uid,Game,Matching,Sid);
					   true->skip
					end,
					if  is_pid(Scene)->
							fun_scene:send_msg(Scene, {off_line,Uid});
						true->skip
					end,	
					update_ets(user_net, Uid, [{#user_net.sid,0},{#user_net.online,?FALSE}]),
					fun_redis:in_game(Uid,Game, out);
				_R->skip
			end;
		_->skip
	end;


do_msg({close_svr}) ->ok;
do_msg(Msg)-> ?log_warning("unknown msg :~p", [Msg]).

do_info({http, Msg}) ->
	?log("--1--fun_sessionMng_do_info"),
	fun_http:async_http_response(Msg);
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
do_info(_Info)->ok.
do_close() -> ?log("fun_sessionmng closed").


send_msg(Msg) ->?send_world(sessionMng,Msg).
call(Msg) ->gen_server:call({global,sessionMng},Msg).

get_onlines()->
	case  get(updateOlCd) of
		Result  when Result==?UNDEFINED orelse Result==0->
			put(updateOlCd,?UPDATEOLCD),
			Onlines=case ets:info(user_net) of
						List  when erlang:is_list(List)->
							{_,Num}=lists:keyfind(size, 1, List),
							Num;
						_->0
					end,
			fun_redisConn:send_msg({onlines,Onlines});
		Num when erlang:is_number(Num)->
			put(updateOlCd,Num-1)
	end.



load_user(Uid,Game)->
	Session=fun_redis:gen_session(Uid),
	case fun_redis:select_dirty_qp([["HMGET",Session,nick,sex,photo,coin]],?USER_DB) of  
		[{ok,[Name,BinSex,Photo,BinCoin]}]when BinCoin=/=?UNDEFINED->
			Coin=util:to_integer(BinCoin), 
			Sex=util:to_integer(BinSex), 
			cache_user(Uid),
			#static_game{item_key=ItemKey}=static_games:get_data(Game),
			Key=fun_redis:gen_item(ItemKey,Uid),
			Skin= case  fun_redis:select_dirty_qp([["HGET",Key,"currSkin"]], ?GAME_ITEM_DB) of  
					  [{ok,BinSkin}] when BinSkin=/=?UNDEFINED->util:to_integer(BinSkin);
					  _->0
				  end , 
			#user_net{id=Uid,name=Name,sex=Sex,photo=Photo,coin=Coin,skin=Skin};
		_R->skip
	end.

cache_user(Uid)->
	List=case get(userIdCache) of  
			 IdCache   when erlang:is_list(IdCache)->IdCache;
			 _->[]
		 end,
	L1=[Uid|lists:delete(Uid,List)],
	Len=length(L1),
	NewList=
		if  Len>10000->
				Outer=lists:nth(Len, L1),
				case  db:get_ets_data(user_net, Outer) of  
					[#user_net{scene=Hid,online=Online,sid=Sid}] ->
						case  erlang:is_pid(Hid) of  
							?TRUE->L1;
							_->
								if  
									Online==?TRUE->?discon(Sid,'cacheFull',10);
									true->skip
								end,
								db:del_ets_data(user_net, Outer),
								lists:delete(Outer, L1)
						end;	
					_->L1
				end;
			true->L1
		end,
	put(userIdCache,NewList).


match_game(Uid,?GAME_SNAKE,Opt,Coin,Sid)->
	case  snake_game:get_data(Opt) of  
		#snake_game{maxPlayer=Max,coin=NeedCoin} when Coin>=NeedCoin ->
			do_match(Uid, ?GAME_SNAKE, Opt, Max, NeedCoin, Sid);
		_->skip
	end;
match_game(Uid,?GAME_TANK,Opt,Coin,Sid)->
	case  tank_game:get_data(Opt) of  
		#tank_game{maxPlayer=Max,coin=NeedCoin} when Coin>=NeedCoin ->
			do_match(Uid, ?GAME_TANK, Opt, Max, NeedCoin, Sid);
		_->skip
	end;	
match_game(Uid,?GAME_BOSSRUN,Opt,Coin,Sid)->
	case  bossRun_game:get_data(Opt) of  
		#bossRun_game{maxPlayer=Max,coin=NeedCoin} when Coin>=NeedCoin ->
			do_match(Uid, ?GAME_BOSSRUN, Opt, Max, NeedCoin, Sid);
		_->skip
	end.
do_match(Uid,Game,Opt,Max,NeedCoin,Sid)->
	#static_game{matchTime=MatchTime}=static_games:get_data(Game),
	case get({matchs,Game,Opt}) of  
		{Time,Users} when erlang:is_list(Users)->
			Len=length(Users),
			case  lists:member(Uid,Users) of  
				?FALSE->
					if  Len+1==Max->
							erase({matchs,Game,Opt}),
							go_to_game([Uid|Users],Game,Opt,NeedCoin);
						true->List=[Uid|Users],
							  put({matchs,Game,Opt},{Time,List}),
							  update_ets(user_net, Uid,[{#user_net.matching,Opt}] ),
							  Fun=fun(Uid)->  
										  {ok,Bin}=pt_writer:write(?PT_MATCHING,{Opt,Len+1,Time-util:unixtime()}),												
										  ?send(ets_fields(user_net,Uid,#user_net.sid),Bin)   
								  end,
							  lists:foreach(Fun, List)
					end;
				_->
					{ok,Bin}=pt_writer:write(?PT_MATCHING,{Opt,Len,Time-util:unixtime()}),												
					?send(Sid,Bin)
			end;
		_R->
			put({matchs,Game,Opt},{util:unixtime()+MatchTime,[Uid]}),
			update_ets(user_net, Uid,[{#user_net.matching,Opt}] ),
			{ok,Bin}=pt_writer:write(?PT_MATCHING,{Opt,1,MatchTime}),												
			?send(Sid,Bin)
	end.	
	
	
quit_match(Uid,Game,Opt,Sid)->
	update_ets(user_net, Uid,[{#user_net.matching,0}] ),
	case get({matchs,Game,Opt}) of  
		{Time,Users} when erlang:is_list(Users)->
			case  lists:member(Uid,Users) of  
				?TRUE->
					List=lists:delete(Uid, Users),
					          if List==[]->erase({matchs,Game,Opt});
								 true-> put({matchs,Game,Opt},{Time,List})
							  end,
							  Fun=fun(Uid)->  
									{ok,Bin}=pt_writer:write(?PT_MATCHING,{Opt,length(List),Time-util:unixtime()}),												
							        ?send(ets_fields(user_net,Uid,#user_net.sid),Bin)   
								  end,
							  lists:foreach(Fun, List),
					{ok,Bin}=pt_writer:write(?RES_QUIT_MATCH,Opt),												
				   ?send(Sid,Bin);
				_->
				   {ok,Bin}=pt_writer:write(?RES_QUIT_MATCH,Opt),												
				   ?send(Sid,Bin)
			end;
		_R->
			{ok,Bin}=pt_writer:write(?RES_QUIT_MATCH,Opt),												
			?send(Sid,Bin)
	end.



clear_match(Now)->
	SnakeOpts=snake_game:get_ids(),
	lists:foreach(fun(Opt)->  
						  #snake_game{minPlayer=Min,coin=NeedCoin}=snake_game:get_data(Opt),
						  do_clear(?GAME_SNAKE, Opt, Now, Min, NeedCoin)
				  end, SnakeOpts),
	TankOpts=tank_game:get_ids(),
	lists:foreach(fun(Opt)->  
						  #tank_game{minPlayer=Min,coin=NeedCoin}=tank_game:get_data(Opt),
						  do_clear(?GAME_TANK, Opt, Now, Min, NeedCoin)
				  end, TankOpts),
    BossRunOpts=bossRun_game:get_ids(),
	lists:foreach(fun(Opt)->  
						  #bossRun_game{minPlayer=Min,coin=NeedCoin}=bossRun_game:get_data(Opt),
						  do_clear(?GAME_BOSSRUN, Opt, Now, Min, NeedCoin)
				  end, BossRunOpts),
	ok.

do_clear(Game,Opt,Now,Min,NeedCoin)->
	case get({matchs,Game,Opt}) of  
		{Time,Users} when Now>Time->
			erase({matchs,Game,Opt}),
			if  length(Users)>=Min->
					go_to_game(Users,Game,Opt,NeedCoin);
				true->
					F1=fun(Uid)-> 
							   update_ets(user_net, Uid,[{#user_net.matching,0}] ),
							   {ok,Bin}=pt_writer:write(?PT_MATCH_FAILED,[]),												
							   ?send(ets_fields(user_net,Uid,#user_net.sid),Bin)   
					   end,
					lists:foreach(F1, Users) 
			end;
		_->skip
	end.





go_to_game(Users,Game,Opt,NeedCoin)->
	Fun=fun(Uid)->  
			[#user_net{name=Name,sid=Sid,coin=Coin,skin=Skin}]=db:get_ets_data(user_net, Uid),
		    update_ets(user_net,Uid,[{#user_net.matching,0},{#user_net.coin,Coin-NeedCoin}]),
			{Uid,Name,Skin,Sid}	
	    end,
	UserData=lists:map(Fun,Users),
	fun_sceneMng:send_msg({create_game,Game,Opt,UserData}).










update_ets(Tab,Key,Fields)->
	case db:get_ets_data(Tab, Key) of  
		 [Data]->
			 Fun=fun({Index,Val},Res)->
						setelement(Index, Res, Val)
				end,
			 NewData=lists:foldl(Fun,Data,Fields),
			 db:put_ets_data(Tab, NewData);
		 _->skip
	end.


ets_fields(Tab,Key,Index) when is_number(Index)->
	case  db:get_ets_data(Tab, Key) of
		[Data]->element(Index,Data);
		_->?UNDEFINED
	end;
ets_fields(Tab,Key,Indexs) when is_list(Indexs) ->
	case  db:get_ets_data(Tab, Key) of
		[Data]->
			Fun=fun(Index,Res)->
						[{Index,element(Index,Data)}|Res]
				end,
			lists:foldl(Fun,[],Indexs);
		_->?UNDEFINED
	end.




