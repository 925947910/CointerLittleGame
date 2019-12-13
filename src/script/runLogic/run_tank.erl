%% @author Administrator
%% @doc @todo Add description to run_tank.


-module(run_tank).
-include("common.hrl").

-record(player,{id=0,name="",sid=0,online=true,die=false,udp=0,score=0,group=0}).
-record(game,{id=0,game=0,status=0}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([init/4,cliEvent/1,process_win/2]).

init(SceneId,Game,Opt,UserData)->
	#tank_game{totalScore=Score,coin=Coin,start=Start,over=Over}=tank_game:get_data(Opt),
    db:put_obj_datas(game, 0, #game{id=SceneId,game=Game}),
	Fun=fun({Uid,Name,Skin,Sid},Res)-> 
				Group=if (length(Res) rem 2) ==0->1;
						 true->2
					  end,
				db:put_obj_datas(player, Uid, #player{id=Uid,name=Name,sid=Sid,score=Score,group=Group}),
				Event={obj,[{"uid",Uid},{"E",?EVENT_PAY},{"pay",Coin},{"game",?GAME_TANK},{"desc",fun_scene:get_recCode()}]},
				fun_redis:add_event(Uid, [Event]),
				[{Uid,Name,Skin,Score,Group}|Res]
		end,
	InitUsers=lists:foldl(Fun, [], UserData),
	{ok,Bin}=pt_writer:write(?PT_INIT_TANK_GAME,{Opt,util:unixtime(),InitUsers}),	
	put(initBin,Bin),
	fun_scene:broadCast(Bin, 0),
	{Start,Over}.



cliEvent(BinData)->
	        case BinData  of  
				<<?CliEventDie:?u8,Bin/binary>>->
					{Uid,_}  = pt:read_int(Bin),
					die(Uid);
				<<?CliEventTankScore:?u8,Uid:?u32,Score:?u32,Attr:?u32,Ascore:?u32>>->
					fun_scene:update_fields(player, Uid, [{#player.score,Score}]),
                    fun_scene:update_fields(player, Attr, [{#player.score,Ascore}]);
                _->skip	
            end.

die(Uid)->
	case  db:get_obj_datas(player, Uid)  of  
		  #player{die=?FALSE}->
			 fun_scene:update_fields(player, Uid, [{#player.die,?TRUE}]),
			 check_win();
		  _->skip
	end.

check_win()->
	Group1=db:filter_class_objs(player, fun(#player{die=Die,group=Group})-> Die==?FALSE andalso Group==1  end),
	Group2=db:filter_class_objs(player, fun(#player{die=Die,group=Group})-> Die==?FALSE andalso Group==2  end),
	case {Group1,Group2} of  
		{Group1,[]}->
			process_win(1, Group1);
		{[],Group2}->
			process_win(2, Group2);
		_->skip
	end.

process_win(Group,Winners)-> 
	Fun= fun(#player{id=Uid,score=Score})->  
			Price=Score,	 
			Event={obj,[{"uid",Uid},{"E",?EVENT_WIN},{"price",Price},{"game",?GAME_TANK},{"desc",fun_scene:get_recCode()}]},
	        fun_redis:add_event(Uid, [Event]),
			<<Uid:?u32,Price:?u32>>
		 end,
	ListBin=lists:map(Fun, Winners),
    UsersLen = length(ListBin),
    UsersBin = list_to_binary(ListBin),	

    F1= fun(#player{id=Uid,name=Name,score=Score})->  
            Price=Score,	 
			{obj,[{"胜利者Id:",Uid},{"胜利者:",Name},{"得分:",Score},{"奖励:",util:to_binary(Price/100)}]}
		 end,
    WinGroup={obj,[{"胜利阵营:",Group},{"胜利玩家:",lists:map(F1, Winners)}]},
	Str=rfc4627:encode({obj,[{"对局详情",WinGroup}]}),
	fun_scene:gameCleanRec(?GAME_TANK, util:to_binary(Str)),

    fun_scene:append_frames(<<?CliEventWin:?u8,Group:?u8,UsersLen:?u16,UsersBin/binary>>),
    fun_scene:sceneStatus(?GAME_OVER, 0).






