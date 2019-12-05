-module(fun_shopMall).
-include("common.hrl").



-define(ITEM_COIN,3).
-define(ITEM_TYPE,4).
-define(ITEM_FIELD,5).
-define(ITEM_DESC,6).

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


do_init() ->ok.

do_time(Now)->ok.

do_call(_,State) ->
	{reply, nomatch, State}.

do_msg({shop_info,Uid,Game,Sid})  ->
	#static_game{shop=Shop,item_key=ItemKey}=static_games:get_data(Game),
	Key=fun_redis:gen_item(ItemKey,Uid),
	case fun_redis:select_dirty_qp([["HMGET",Key,"currSkin","Skins"]], ?GAME_ITEM_DB) of  
		[{ok,[BinSkin,BinSkins]}]->
			Skin=if  BinSkin==?UNDEFINED->0;
					 true->util:to_integer(BinSkin)
				 end,		 
			Skins=if  BinSkins==?UNDEFINED->[];
					  true->{ok,Json,_}=rfc4627:decode(BinSkins),
							Json
				  end,
			Items=Skins,
			Ids=Shop:get_ids(),
			Fun=fun(Id)->   
						ShopData=Shop:get_data(Id),
						Has=case  lists:member(Id, Items)of
								?TRUE ->?Btrue;
								_ ->?Bfalse
							end,
						{Id,element(?ITEM_COIN, ShopData),Has}	
				end,
			ShopDatas=lists:map(Fun, Ids),
			{ok,Bin}=pt_writer:write(?RES_SHOP_INFO,{Skin,ShopDatas}),												
			?send(Sid,Bin);
		_->skip
	end;

do_msg({shop_buy,Uid,Game,ItemId,Sid})  ->
	#static_game{shop=Shop,item_key=ItemKey}=static_games:get_data(Game),
	ShopData=Shop:get_data(ItemId),
	NeedCoin=element(?ITEM_COIN, ShopData),
	Key=fun_redis:gen_item(ItemKey,Uid),
	Field=element(?ITEM_FIELD, ShopData),
	Desc=element(?ITEM_DESC, ShopData),
	case fun_redis:select_dirty_qp([["HGET",Key,Field]], ?GAME_ITEM_DB) of  
		[{ok,BinItems}]->
			Items=if  BinItems==?UNDEFINED->[];
					  true->{ok,Json,_}=rfc4627:decode(BinItems),
							Json
				  end,	
			case lists:member(ItemId, Items)  of  
				?FALSE->
					Session=fun_redis:gen_session(Uid),
					Coin=  case fun_redis:select_dirty_qp([["HGET",Session,coin]],?USER_DB) of  
							   [{ok,BinCoin}] when BinCoin=/=?UNDEFINED ->
								   util:to_integer(BinCoin);
							   _->0
						   end,
					if  Coin>NeedCoin->
							Event={obj,[{"uid",Uid},{"E",?EVENT_PAY},{"pay",NeedCoin},{"game",Game},{"desc",Desc}]},
							fun_redis:add_event(Uid, [Event]),
							NewItems=rfc4627:encode(Items++[ItemId]),
							fun_redis:select_dirty_qp([["HSET",Key,Field,NewItems]], ?GAME_ITEM_DB), 
							{ok,Bin}=pt_writer:write(?RES_SHOP_BAY,{ItemId,?Btrue,Coin-NeedCoin}),												
							?send(Sid,Bin);
						true->skip
					end;
				_->skip
			end;
		_->skip
	end;

do_msg({use_skin,Uid,Game,SkinId,Sid})  ->
	#static_game{item_key=ItemKey}=static_games:get_data(Game),
	Key=fun_redis:gen_item(ItemKey,Uid),
	fun_redis:select_dirty_qp([["HSET",Key,"currSkin",SkinId]], ?GAME_ITEM_DB),
	update_ets(user_net, Uid, [{#user_net.skin,SkinId}]),
	{ok,Bin}=pt_writer:write(?RES_USE_SKIN,{SkinId,?Btrue}),												
	?send(Sid,Bin);

do_msg(Msg)-> ?log_warning("shopMall unknown msg :~p", [Msg]).

do_info({http, Msg}) ->
	?log("shopMall_do_info"),
	fun_http:async_http_response(Msg);
do_info({timeout, _TimerRef, CallBackInfo}) ->
	%% 	erlang:start_timer(Time, Dest, Msg),
	case CallBackInfo of
		{Module, Function} ->
			apply(Module, Function, []);
		{Module, Function, Args} ->
			apply(Module, Function, Args);
		_ ->
			?log_warning("shopMall unknown timer callback,CallbackInfo=~p", [CallBackInfo])
	end;
do_info(_Info)->ok.
do_close() -> ?log("fun_shopMall closed").


send_msg(Msg) ->?send_world(shopMall,Msg).
call(Msg) ->gen_server:call({global,shopMall},Msg).








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











