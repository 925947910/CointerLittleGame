%% @author Administrator
%% @doc @todo Add description to run_snake.


-module(run_snake).
-include("common.hrl").

-record(player,{id=0,name="",sid=0,online=true,die=false,udp=0,score=0}).
-record(game,{id=0,game=0,status=0,pricePool=0}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([init/4,cliEvent/1,process_win/2,test/1]).

init(SceneId,Game,Opt,UserData)->
	#snake_game{totalScore=Score,coin=Coin}=snake_game:get_data(Opt),
	PlayerNum=length(UserData),
    db:put_obj_datas(game, 0, #game{id=SceneId,game=Game,pricePool=(Coin*PlayerNum*8) div 10}),
	Fun=fun({Uid,Name,_Skin,Sid},Res)-> 
				db:put_obj_datas(player, Uid, #player{id=Uid,name=Name,sid=Sid}),
				Event={obj,[{"uid",Uid},{"E",?EVENT_PAY},{"pay",Coin},{"game",?GAME_SNAKE},{"desc",fun_scene:get_recCode()}]},
				fun_redis:add_event(Uid, [Event]),
				[Uid|Res]
		end,
	Uids=lists:foldl(Fun, [], UserData),
	{ok,Bin}=pt_writer:write(?PT_INIT_SNAKE_GAME,{Opt,util:unixtime(),Score,UserData}),		
	fun_scene:broadCast(Bin, Uids),
	put(initBin,Bin),
	#snake_game{start=Start,over=Over}=snake_game:get_data(Opt),
	{Start,Over}.



cliEvent(BinData)->
	case BinData  of  
		<<?CliEventDie:?u8,Bin/binary>>->
			{Uid,_}  = pt:read_int(Bin),
			die(Uid);
		<<?CliEventBeam:?u8,Bin/binary>>->
			{DataLen,DataBody}  = pt:read_short(Bin),
			CurrBinLen=DataLen*6,
			<<CurrBin:CurrBinLen/binary,_/binary>>=DataBody,
			Fun=fun(<<Uid:?u32,Score:?u16,BinRemain/binary>>)-> fun_scene:update_fields(player, Uid, [{#player.score,Score}]), {{Uid,Score},BinRemain} end,
            fun_scene:extractBin(CurrBin, Fun, []);
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
	case db:filter_class_objs(player, fun(#player{die=Die})-> Die==?FALSE end)  of  
		[]->process_win(0, 0);
		[#player{id=Uid,score=Score}]->
			process_win(Uid, Score);
		_->skip
	end.

process_win(0,_)->
	Str=rfc4627:encode({obj,[{"对局详情","平局"}]}),
	fun_scene:gameCleanRec(?GAME_SNAKE, Str),
    fun_scene:append_frames(<<?CliEventWin:?u8,0:?u32,0:?u32>>),
    fun_scene:sceneStatus(?GAME_OVER, 0);

process_win(Uid,Score)-> 
	#game{game=GameId,pricePool=PricePool,status=?GAME_RUNING}=db:get_obj_datas(game, 0),
	#snake_game{totalScore=TotalScore}=snake_game:get_data(GameId),
	Price=util:floor((Score/TotalScore)*(PricePool*3/8))+util:floor(PricePool*5/8),
	Event={obj,[{"uid",Uid},{"E",?EVENT_WIN},{"price",Price},{"game",?GAME_SNAKE},{"desc",fun_scene:get_recCode()}]},
	fun_redis:add_event(Uid, [Event]),
	Name=fun_scene:get_fields(player, Uid, #player.name),
	Winners={obj,[{"胜利者Id",Uid},{"胜利者",Name},{"得分",Score},{"奖励",util:to_binary(Price/100)}]},
	Str=rfc4627:encode({obj,[{"对局详情",Winners}]}),
	fun_scene:gameCleanRec(?GAME_SNAKE, util:to_binary(Str)),
	fun_scene:append_frames(<<?CliEventWin:?u8,Uid:?u32,Price:?u32>>),
    fun_scene:sceneStatus(?GAME_OVER, 0).

test(Key)->
	Str=rfc4627:encode({obj,[{"对局详情",<<"">>}]}),
	fun_redis:select_dirty_qp([["set",Key,Str]], 10).
