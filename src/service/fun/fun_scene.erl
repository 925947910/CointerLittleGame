-module(fun_scene).


-include("common.hrl").



-define(GameIndexId,2).
-define(GameIndexGame,3).
-define(GameIndexStatus,4).


-define(PlayerIndexId,2).
-define(PlayerIndexName,3).
-define(PlayerIndexSid,4).
-define(PlayerIndexOnline,5).
-define(PlayerIndexDie,6).
-define(PlayerIndexUdp,7).
-export([on_init/1,on_close/2,do_time/1,do_msg/1,do_call/2,send_msg/2]).
-export([broadCast/2,extractBin/3,append_frames/1,processFrames/0,sceneStatus/2]).
-export([update_fields/3,get_fields/3]).
on_init({SceneId,GameId,Opt,UserData})->
	{Start,Over}= mod_run(init, [SceneId,GameId,Opt,UserData]),
	{ok,Bin}=pt_writer:write(?PT_START_GAME,[]),
	erlang:start_timer(Start, self(),{?MODULE,sceneStatus,[?GAME_RUNING,Bin]}),
	erlang:start_timer(Over, self(),{?MODULE,sceneStatus,[?GAME_OVER,0]}).

on_close(Id,CloseReason) -> 
	fun_sceneMng:send_msg({scene_del,Id,CloseReason}).
	
sceneStatus(?GAME_RUNING,Bin)->	
	broadCast(Bin,0),
	update_fields(game, 0, [{?GameIndexStatus,?GAME_RUNING}]);
sceneStatus(?GAME_OVER,_)->	
	processFrames(),
	update_fields(game, 0, [{?GameIndexStatus,?GAME_OVER}]).


do_call(_,State) ->	
	{reply, nomatch, State}.
%%  10fps 
do_time(_Now) ->
	processFrames(),
	ok.

write_frames(FramesId,Actions)->
	case  get('FramesIds') of  
		  List when erlang:is_list(List)->put('FramesIds',List++[FramesId]);
		  _->put('FramesIds',[FramesId])
	end,
	put({frames,FramesId},Actions).


do_msg({sceneUdp,Host,Port,Data})->
	 SceneId=get_fields(game, 0,?GameIndexId),
	case Data  of  
		<<Len:?u32,Remain/binary >> when byte_size(Remain)==Len->
             case Remain of  
                 <<Uid:?u32,SceneId:?u32,?UDP_INIT_CONN:?u8>> ->
                  update_fields(player, Uid, [{?PlayerIndexUdp,#user_udp{uid=Uid,ip=Host,port=Port}}]);
                 _->skip
             end;
		_->skip
	end;

do_msg({reconn,Uid,Sid})->
	update_fields(player, Uid, [{?PlayerIndexSid,Sid}]);

do_msg({losed_frames,FramesId,Uid}) ->
	 case get({frames,FramesId})  of  
		 Actions when erlang:is_list(Actions)->
			 {ok,Bin}=pt_writer:write(?RES_LOSED_FRAMES,{FramesId,Actions}),
			 broadCast(Bin, Uid);
		 _->skip
	 end;
do_msg({cliAction,BinData}) ->
	case get_fields(game, 0, ?GameIndexStatus) of  
		?GAME_RUNING->
			append_frames(BinData);
		_->skip
	end;
do_msg({cliEvent,BinData}) ->
	case get_fields(game, 0, ?GameIndexStatus) of  
		?GAME_RUNING->
			append_frames(BinData),
			mod_run(cliEvent, [BinData]);
         _->skip
     end;
do_msg({quit_game,Uid}) ->quit_game(Uid);
do_msg({off_line,Uid})->off_line(Uid); 

do_msg(_Msg) ->
	?debug("unknown msg,msg=~p",[_Msg]),
	ok.
quit_game(Uid)->
	case  get_fields(player, Uid,[?PlayerIndexDie,?PlayerIndexOnline])  of  
		[{_,?TRUE},{_,?TRUE}]->
			update_fields(player, Uid, [{?PlayerIndexOnline,?FALSE}]),
			append_frames(<<?CliEventQuit:?u8,Uid:?u32>>),
            SceneId=get_fields(game, 0,?GameIndexId),
            fun_sceneMng:send_msg({quit_game,SceneId,Uid});
		_->skip
	end.

off_line(Uid)->
	case get_fields(player, Uid, ?PlayerIndexOnline) of  
		    ?TRUE->
			update_fields(player, Uid, [{?PlayerIndexOnline,?FALSE}]),
			append_frames(<<?CliEventOffLine:?u8,Uid:?u32>>),
            SceneId=get_fields(game, 0,?GameIndexId),
            fun_sceneMng:send_msg({off_line,SceneId,Uid});
		_->skip
	end.








send_msg(Sid,Msg)->
	gen_server:cast(Sid, {msg, Msg}).




send_broadCast(Users,Bin)->
	Fun=fun(Player)-> 
				Uid=element(?PlayerIndexId, Player),
                Sid=element(?PlayerIndexSid, Player),
                Online=element(?PlayerIndexOnline, Player),
				case Online of 
					?TRUE when erlang:is_pid(Sid)->
						?broadCast(Sid,Uid,Bin);
					_->skip
				end	  end,
	lists:foreach(Fun, Users).
broadCast(Bin,0)->
	case  db:get_class_objs(player) of  
		Users  when  erlang:is_list(Users)->
			send_broadCast(Users, Bin);
		_->skip
	end;
broadCast(Bin,Uid)when erlang:is_integer(Uid)->
	case  db:get_obj_datas(player, Uid)  of  
		User when erlang:is_tuple(User)->
			send_broadCast([User], Bin);
		_->skip
	end;
broadCast(Bin,Uids)when erlang:is_list(Uids)->
	Fun= fun(Uid,Res)-> 
				 case  db:get_obj_datas(player, Uid)  of  
					 User when erlang:is_tuple(User)->
						 [User|Res];
					 _->Res
				 end  end,
	send_broadCast(lists:foldl(Fun, [], Uids), Bin);
broadCast(Bin,Fun)->
	case  db:filter_class_objs(player,Fun)  of  
		Receivers  when  erlang:is_list(Receivers)->
			send_broadCast(Receivers, Bin);
		_->skip
	end.



update_fields(Tab,Key,Fields)->
	case db:get_obj_datas(Tab, Key) of  
		 Data when erlang:is_tuple(Data)->
			 Fun=fun({Index,Val},Res)->
						setelement(Index, Res, Val)
				end,
			 NewData=lists:foldl(Fun,Data,Fields),
			 db:put_obj_datas(Tab, Key, NewData);
		 _->skip
	end.

get_fields(Tab,Id,Index) when is_number(Index)->
	case db:get_obj_datas(Tab,Id)of
		Data when erlang:is_tuple(Data)->
			element(Index,Data);
		_->skip
	end;
get_fields(Tab,Id,Indexs) when is_list(Indexs) ->
	case db:get_obj_datas(Tab,Id)of
		Data when erlang:is_tuple(Data)->
			Fun=fun(Index,Res)->
						[{Index,element(Index,Data)}|Res]
				end,
			lists:foldl(Fun,[],Indexs);
		_->skip
	end.




genFramesId()->
	case  get(framesId)  of  
		?UNDEFINED->
			put(framesId,2),
			1;
		Id->put(framesId,Id+1),
			Id
	end.


append_frames(BinData)->
	{FramesId,Actions}=case get(currFrames)  of  
						   {CurrFramesId,CurrActions}->{CurrFramesId,CurrActions};
						   _->{genFramesId(),[]}
					   end,
	NewActions=Actions++[BinData],
	put(currFrames,{FramesId,NewActions}).

extractBin(<<>>,_,Res)->lists:reverse(Res);
extractBin(Bin,Fun,Res)->
	{Data,RemainBin}=Fun(Bin),
	extractBin(RemainBin,Fun,[Data|Res]).

processFrames()->
	case get_fields(game, 0, ?GameIndexStatus) of  
		?GAME_RUNING->
			case get(currFrames)  of  
				{FramesId,Actions}->
					{ok,Bin}=pt_writer:write(?PT_SYNC_FRAMES,{FramesId,Actions}),
					broadCast(Bin, 0),
					write_frames(FramesId, Actions);
				_->skip	
			end,
			NewFramesId=genFramesId(),
			put(currFrames,{NewFramesId,[]});
		?GAME_OVER-> erlang:send_after(1, self(), {stop,'GameOver'});
		_->skip
	end.

mod_run(Fun,Args)->
	Mod=get(run_mod),
	apply(Mod, Fun, Args).
