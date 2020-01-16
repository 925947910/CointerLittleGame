%% @author Administrator
%% @doc @todo Add description to run_snake.


-module(run_snake).
-include("common.hrl").

-record(player,{id=0,name="",sid=0,online=true,die=false,udp=0,score=0,kill=0}).
-record(game,{id=0,game=0,status=0,pricePool=0}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([init/4,cliEvent/1,process_win/2,test/1,gameOver/0]).

init(SceneId,Game,Opt,UserData)->
	#snake_game{totalScore=Score,coin=Coin}=snake_game:get_data(Opt),
	PlayerNum=length(UserData),
	PricePool=(Coin*PlayerNum*8) div 10,
    db:put_obj_datas(game, 0, #game{id=SceneId,game=Game,pricePool=PricePool}),
	Fun=fun({Uid,Name,_Skin,Sid},Res)-> 
				db:put_obj_datas(player, Uid, #player{id=Uid,name=Name,sid=Sid}),
				Event={obj,[{"uid",Uid},{"E",?EVENT_PAY},{"pay",Coin},{"game",?GAME_SNAKE},{"desc",fun_scene:get_recCode()}]},
				fun_redis:add_event(Uid, [Event]),
				[Uid|Res]
		end,
	Uids=lists:foldl(Fun, [], UserData),
	
	EventRebate={obj,[{"uid",?PLAT_REBATE_UID},{"E",?EVENT_PLAT_REBATE},{"pay",Coin*PlayerNum-PricePool},{"game",?GAME_SNAKE},{"desc",fun_scene:get_recCode()}]},
	fun_redis:add_event(?PLAT_REBATE_UID, [EventRebate]),
	
	{ok,Bin}=pt_writer:write(?PT_INIT_SNAKE_GAME,{Opt,util:unixtime(),Score,UserData}),		
	fun_scene:broadCast(Bin, Uids),
	put(initBin,Bin),
	#snake_game{start=Start,over=Over}=snake_game:get_data(Opt),
	{Start,Over}.



cliEvent(BinData)->
	case BinData  of  
		<<?CliEventDie:?u8,Bin/binary>>->
			{Uid,B1}  = pt:read_int(Bin),
			{Killer,_}  = pt:read_int(B1),
			die(Uid,Killer);
		<<?CliEventBeam:?u8,Bin/binary>>->
			{DataLen,DataBody}  = pt:read_short(Bin),
			CurrBinLen=DataLen*6,
			<<CurrBin:CurrBinLen/binary,_/binary>>=DataBody,
			Fun=fun(<<Uid:?u32,Score:?u16,BinRemain/binary>>)-> fun_scene:update_fields(player, Uid, [{#player.score,Score}]), {{Uid,Score},BinRemain} end,
            fun_scene:extractBin(CurrBin, Fun, []);
         _->skip	
    end.

die(Uid,Killer)->
	case  db:get_obj_datas(player, Uid)  of  
		  #player{die=?FALSE}->
			 fun_scene:update_fields(player, Uid, [{#player.die,?TRUE}]),
			 fun_scene:update_fields(player, Killer, [{#player.kill,fun_scene:get_fields(player, Killer, #player.kill)+1}]),
			 check_win();
		  _->skip
	end.

check_win()->
	case db:filter_class_objs(player, fun(#player{die=Die})-> Die==?FALSE end)  of  
		[#player{id=Uid,score=Score}]->
			process_win(Uid, Score);
		_->skip
	end.
process_win(Uid,Score)-> 
	update_kills(),
	#game{pricePool=PricePool}=db:get_obj_datas(game, 0),
	Price=PricePool,
	Event={obj,[{"uid",Uid},{"E",?EVENT_WIN},{"price",Price},{"game",?GAME_SNAKE},{"desc",fun_scene:get_recCode()}]},
	fun_redis:add_event(Uid, [Event]),
	Name=fun_scene:get_fields(player, Uid, #player.name),
	Winners={obj,[{"胜利者Id",Uid},{"胜利者",Name},{"得分",Score},{"奖励",util:to_binary(Price/100)}]},
	Str=rfc4627:encode({obj,[{"对局详情",Winners}]}),
	fun_scene:gameCleanRec(?GAME_SNAKE, util:to_binary(Str)),
	fun_scene:append_frames(<<?CliEventWin:?u8,Uid:?u32,Price:?u32>>),
    fun_scene:sceneStatus(?GAME_OVER, 0).

gameOver()->
	update_kills(),
	#game{pricePool=PricePool}=db:get_obj_datas(game, 0),
	Winners=case db:filter_class_objs(player, fun(#player{die=Die})-> Die==?FALSE end)  of  
		Players when erlang:is_list(Players)->
			Price=(PricePool-eatBeam(0)) div length(Players),
			Fun=fun(#player{id=Uid,name=Name,score=Score})-> 
					Event={obj,[{"uid",Uid},{"E",?EVENT_WIN},{"price",Price},{"game",?GAME_SNAKE},{"desc",fun_scene:get_recCode()}]},	
				    fun_redis:add_event(Uid, [Event]),
					{obj,[{"存活者Id",Uid},{"存活者",Name},{"得分",Score},{"奖励",util:to_binary(Price/100)}]}
				end,
			lists:map(Fun, Players);
		_->Price=0,[]
	end,
	Str=rfc4627:encode({obj,[{"对局详情",Winners}]}),
	fun_scene:gameCleanRec(?GAME_SNAKE, util:to_binary(Str)),
	fun_scene:append_frames(<<?CliEventWin:?u8,0:?u32,Price:?u32>>),
    fun_scene:sceneStatus(?GAME_OVER, 0).
	


update_kills()->
	case db:get_class_objs(player) of  
		Players->
			Fun=fun(#player{id=Uid,kill=Kill})->
						if Kill=/=0->
							   Session=fun_redis:gen_session(Uid),
							   Arg1=[["HINCRBY" ,Session ,totalKill, Kill],["HINCRBY" ,Session ,gameTimes, 1]],
							   Arg2=["HSET",Session ,maxKill,Kill],
							   Args=case  fun_redis:select_dirty_qp([["HGET" ,Session ,maxKill]], ?USER_DB) of  
										[{ok,MaxKillBin}] when MaxKillBin=/=?UNDEFINED->
											MaxKill=util:to_integer(MaxKillBin),
											if MaxKill<Kill->[Arg2|Arg1];
											   true->Arg1
											end;
										_->[Arg2|Arg1]
									end , 
							   fun_redis:select_dirty_qp(Args, ?USER_DB);
						   true->skip
						end	
				end,
			lists:foreach(Fun, Players);
		_->skip
	end.

eatBeam(Score)->
case get(eatBeam) of  
	Beam when erlang:is_integer(Beam)->put(eatBeam,Beam+Score),Beam+Score;
	 _->put(eatBeam,Score),
		Score
end.

test(Key)->
	Str=rfc4627:encode({obj,[{"对局详情",<<"">>}]}),
	fun_redis:select_dirty_qp([["set",Key,Str]], 10).
