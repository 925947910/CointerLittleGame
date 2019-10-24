-module(pt_writer).
-export([write/2]).
-include("common.hrl").


%% Universal===========================================================

%% 1000
write(?PT_PING,_)->add_head(?PT_PING, <<>>);

%% 2001
write(?RES_LOGIN,{Result,Name,Sex,Photo,Coin,Skin}) ->
	 add_head(?RES_LOGIN, << Result:?u8,?str(Name),Sex:?u8,?str(Photo),Coin:?u32,Skin:?u8>>);

%% 3001
write(?PT_MATCHING,{GameId,Len,Time}) ->
	 add_head(?PT_MATCHING,<<GameId:?u8,Len:?u8,Time:?u8>>);
%% 3002
write(?PT_MATCH_FAILED,_) ->
	 add_head(?PT_MATCH_FAILED,<<>>);
%% 2003
write(?RES_QUIT_MATCH,GameId) ->
	 add_head(?RES_QUIT_MATCH,<<GameId:?u8>>);

%% 2004
write(?RES_SHOP_INFO,{Skin,ShopDatas}) ->
	Fun=fun({Id,Coin,Own})->  
		<<Id:?u8,Coin:?u16,Own:?u8>>
		end,
	ListBin = [Fun(D)||D<-ShopDatas],
    ShopsLen = length(ListBin),
    ShopsBin = list_to_binary(ListBin),
	 add_head(?RES_SHOP_INFO,<<Skin:?u8,ShopsLen:?u16,ShopsBin/binary>>);
%%2005 
write(?RES_SHOP_BAY,{Id,Result,Coin}) ->
	 add_head(?RES_SHOP_BAY,<<Id:?u8,Result:?u8,Coin:?u32>>);

%% 2006
write(?RES_USE_SKIN,{Id,Result}) ->
	 add_head(?RES_USE_SKIN,<<Id:?u8,Result:?u8>>);
%% 2007
write(?RES_LOSED_FRAMES,{FramesId,Actions}) ->
	Len = length(Actions),
	BinData = list_to_binary(Actions),
	add_head(?RES_LOSED_FRAMES,<<FramesId:?u32,Len:?u16,BinData/binary>>);
%% 3003
write(?PT_INIT_SNAKE_GAME,{GameId,Seed,Score,UserData}) ->
	Fun=fun({Uid,Name,Skin,_})->  
				<<Uid:?u32,?str(Name),Skin:?u8>>
		end,
	ListBin = [Fun(D)||D<-UserData],
    UsersLen = length(ListBin),
    UsersBin = list_to_binary(ListBin),
	 add_head(?PT_INIT_SNAKE_GAME,<<GameId:?u8,Seed:?u32,Score:?u32,UsersLen:?u16,UsersBin/binary>>);
%% 3009
write(?PT_INIT_TANK_GAME,{GameId,Seed,UserData}) ->
	Fun=fun({Uid,Name,Skin,Score,Group})->  
				<<Uid:?u32,?str(Name),Skin:?u8,Score:?u32,Group:?u8>>
		end,
	ListBin = [Fun(D)||D<-UserData],
    UsersLen = length(ListBin),
    UsersBin = list_to_binary(ListBin),
	 add_head(?PT_INIT_TANK_GAME,<<GameId:?u8,Seed:?u32,UsersLen:?u16,UsersBin/binary>>);
%% 3004
write(?PT_START_GAME,_) ->
	 add_head(?PT_START_GAME,<<>>);

%% 3006
write(?PT_SYNC_FRAMES,{FramesId,Actions}) ->
	Len = length(Actions),
	BinData = list_to_binary(Actions),
	add_head(?PT_SYNC_FRAMES,<<FramesId:?u32,Len:?u16,BinData/binary>>);


write(PT,DATA) ->
	?log("~p:no match PT=~p,DATA=~p",[?MODULE,PT,DATA]).




%% 1:1表示后面没数据了0:表示后面还有数据

%% 2:保留 
%% 3:
%% 0x0表示附加数据帧
%% 0x1表示文本数据帧
%% 0x2表示二进制数据帧
%% 0x3-7暂时无定义，为以后的非控制帧保留
%% 0x8表示连接关闭
%% 0x9表示ping
%% 0xA表示pong
%% 0xB-F暂时无定义，为以后的控制帧保留
%% 4: 
%% 掩码处理1：表示处理过 0：表示没处理过

add_head(PT,0) ->
	LenBin = bin_len(2),
	{ok,[<< 1:1, 0:3, 2:4, 0:1, LenBin/bits>>,<<PT:?u16>>]};

add_head(PT,BinData) ->
	Len = iolist_size(BinData),
	LenBin = bin_len(Len+2),
	{ok,[<< 1:1, 0:3, 2:4, 0:1, LenBin/bits>>,<<PT:?u16,BinData/binary>>]}.

bin_len(N) ->
	case N of
		N when N =< 125 -> << N:7 >>;
		N when N =< 16#ffff -> << 126:7, N:16 >>;
		N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
	end.

