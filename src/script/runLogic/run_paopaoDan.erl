%% @author Administrator
%% @doc @todo Add description to run_tank.


-module(run_paopaoDan).
-include("common.hrl").

-record(player,{id=0,name="",sid=0,online=true,die=false,udp=0,score=0,group=0}).
-record(game,{id=0,game=0,status=0}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([init/4,cliEvent/1,process_win/2]).

init(SceneId,Game,Opt,UserData)->
	#paopaoDan_game{totalScore=_Score,start=Start,over=Over}=paopaoDan_game:get_data(Opt),
    db:put_obj_datas(game, 0, #game{id=SceneId,game=Game}),
	Fun=fun({Uid,Name,Skin,Sid},Res)-> 
				Group=if (length(Res) rem 2) ==0->1;
						 true->2
					  end,
				db:put_obj_datas(player, Uid, #player{id=Uid,name=Name,sid=Sid,group=Group}),
				[{Uid,Name,Skin}|Res]
		end,
	InitUsers=lists:foldl(Fun, [], UserData),
	{ok,Bin}=pt_writer:write(?PT_INIT_PAOPAODAN_GAME,{Opt,util:unixtime(),fun_scene:get_recCode(),InitUsers}),	
	put(initBin,Bin),
	fun_scene:broadCast(Bin, 0),
	{Start,Over}.



cliEvent(BinData)->
	case BinData  of  
		<<?CliEventDie:?u8,Bin/binary>>->
			{Uid,_}  = pt:read_int(Bin),
			over(Uid);
		_->skip	
	end.


over(Uid)->
	case  db:get_obj_datas(player, Uid)  of  
		  #player{die=?FALSE}->
			 fun_scene:update_fields(player, Uid, [{#player.die,?TRUE}]),
			 check_win();
		  _->skip
	end.

check_win()->
	case db:filter_class_objs(player, fun(#player{die=Die})-> Die==?FALSE end)  of  
		[#player{id=Uid}]->process_win(Uid, 0);
		_->skip
	end.

process_win(Uid,_)->
	fun_scene:append_frames(<<?CliEventWin:?u8,Uid:?u32,0:?u32>>),
    fun_scene:sceneStatus(?GAME_OVER, 0).







