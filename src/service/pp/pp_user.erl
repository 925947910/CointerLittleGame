-module(pp_user).
-export([handle/4]).
-include("common.hrl").


handle(_,login_game,{_,Code,_}=Data,_Ip) ->
	if Code=/="" andalso Code=/=?UNDEFINED ->
		   fun_sessionMng:send_msg({login_game,Data,self()});
	   true-> skip
	end;
handle(_,match_game,Opt,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_sessionMng:send_msg({match_game,Uid,Opt,self()});
		_->skip
	end;
handle(_,quit_match,_,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_sessionMng:send_msg({quit_match,Uid,self()});
		_->skip
	end;

handle(_,game_action,BinData,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_sessionMng:send_msg({sceneMsg,Uid,{cliAction,BinData}});
		_->skip
	end;
handle(_,game_even,BinData,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_sessionMng:send_msg({sceneMsg,Uid,{cliEvent,BinData}});
		_->skip
	end;
handle(_,shop_info,GameId,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_shopMall:send_msg({shop_info,Uid,GameId,self()});
		_->skip
	end;

handle(_,shop_buy,{GameId,SkinId},_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_shopMall:send_msg({shop_buy,Uid,GameId,SkinId,self()});
		_->skip
	end;

handle(_,use_skin,{GameId,SkinId},_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_shopMall:send_msg({use_skin,Uid,GameId,SkinId,self()});
		_->skip
	end;
handle(_,losed_frames,FramesId,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_sessionMng:send_msg({sceneMsg,Uid,{losed_frames,FramesId,Uid}});
		_->skip
	end;
handle(_,quit_game,_,_Ip) ->
	case get(uid)  of  
		 Uid when erlang:is_integer(Uid)->	
			 fun_sessionMng:send_msg({sceneMsg,Uid,{quit_game,Uid}});
		_->skip
	end;

handle(_,_Fun,_Data,_Ip) -> ?log("pp_usr unmatch!!"),error.


