-module(fun_sceneMng).
-include("common.hrl").
-export([do_init/0,do_close/0,do_call/1,do_info/1,do_time/1,do_msg/1,send_msg/1]).
-define(SCENE_RUN_ON_TIME,1000).
-define(AUTO_RENEW_CD,60).
-define(AliveTime,3600*24).
do_init()-> 
	db:init_ets_table(scene, 2).
do_close() -> ok.

do_call(_Msg) -> ok.
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
do_info(_Info) -> ok.






  do_msg({create_game,GameId,Opt,UserData})-> 
	  SceneId=genSceneId(),
	  Fun=fun({Uid,_,_,_})-> Uid end,
	  Uids=lists:map(Fun, UserData),
	  db:put_ets_data(scene, #scene{id=SceneId,hid=0,owner=0,game=GameId,players=Uids}),
	  scene_sup:add({SceneId,GameId,Opt,UserData});

  do_msg({scene_add,SceneId,SceneHid})-> 
	  case db:get_ets_data(scene,SceneId) of  
		  [#scene{players=Players}=Scene]->
			  db:put_ets_data(scene, Scene#scene{hid=SceneHid}),
			  fun_sessionMng:send_msg({in_scene,SceneHid,Players});
		  _-> skip
	  end;


  do_msg({scene_del,SceneId,_CloseReason})-> 
	  case db:get_ets_data(scene,SceneId) of  
		  [#scene{players=Players,hid=SceneHid}]->
			  db:del_ets_data(scene, SceneId),
              fun_sessionMng:send_msg({out_scene,SceneHid,Players});
		  _->skip
	  end;


    do_msg({quit_game,SceneId,Uid})-> 
	  case db:get_ets_data(scene,SceneId) of  
		  [#scene{hid=SceneHid,players=Players}=Scene]->
			   db:put_ets_data(scene, Scene#scene{players=Players--[Uid]}),
			   fun_sessionMng:send_msg({out_scene,SceneHid,[Uid]});
		  _->skip
	  end;
  
  do_msg({off_line,SceneId,Uid})-> 
	  case db:get_ets_data(scene,SceneId) of  
		  [#scene{hid=SceneHid,players=Players}=Scene]->
			   db:put_ets_data(scene, Scene#scene{players=Players--[Uid]}),
			   fun_sessionMng:send_msg({out_scene,SceneHid,[Uid]});
		  _->skip
	  end;

do_msg(Msg) ->
	?debug("unknown msg,msg=~p",[Msg]).

do_time(_Now) -> 
%% 	?log("fun_sceneMng:do_time~p:",[_Now]),
	?SCENE_RUN_ON_TIME.



send_msg(Msg) ->?send_world(sceneMng,Msg).




	
genSceneId()->
	case  get('NewSceneId')  of  
		Id when erlang:is_integer(Id)->
			put('NewSceneId',Id+1),
			Id;
		_->put('NewSceneId',2),
		   1  
	end.









