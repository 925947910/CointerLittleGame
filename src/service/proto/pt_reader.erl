-module(pt_reader).
-export([read/2]).
-include("common.hrl").

%% Universal===========================================================

%% Client2GameSvr======================================================
%% 1001
read(?REQ_LOGIN,<<Bin/binary>>) ->
        {Uid,B1}  = pt:read_int(Bin),
		{Code,B2} = pt:read_string(B1),
		{Game,_} = pt:read_byte(B2),
		?log("!!!!!!!!!!!!!!!!!~p",[{Uid,Code,Game}]),
{ok,pp_user,login_game,{Uid,Code,Game}};

%% 1002
read(?REQ_MATCH_GAME,<<Bin/binary>>) ->
        {Opt,_} = pt:read_byte(Bin),
{ok,pp_user,match_game,Opt};


%% 1003
read(?REQ_QUIT_MATCH,<<Bin/binary>>) ->
{ok,pp_user,quit_match,[]};

%% 1004
read(?REQ_SHOP_INFO,<<Bin/binary>>) ->
	{Game,_} = pt:read_byte(Bin),
{ok,pp_user,shop_info,Game};

%% 1005
read(?REQ_SHOP_BUY,<<Bin/binary>>) ->
	{Game,B1} = pt:read_byte(Bin),
	{ItemId,_} = pt:read_byte(B1),
{ok,pp_user,shop_buy,{Game,ItemId}};

%% 1006
read(?REQ_USE_SKIN,<<Bin/binary>>) ->
	{Game,B1} = pt:read_byte(Bin),
	{SkinId,_} = pt:read_byte(B1),
{ok,pp_user,use_skin,{Game,SkinId}};
%% 1007
read(?REQ_LOSED_FRAMES,<<Bin/binary>>) ->
	{FramesId,_} = pt:read_int(Bin),
{ok,pp_user,losed_frames,FramesId};
%% 3008
read(?PT_QUIT_GAME,<<Bin/binary>>) ->
{ok,pp_user,quit_game,0};


%% 3005
read(?PT_GAME_ACTION,<<Bin/binary>>) ->
{ok,pp_user,game_action,Bin};

%% 3007
read(?PT_GAME_EVENT,<<Bin/binary>>) ->
{ok,pp_user,game_even,Bin};

read(_Cmd, _R) ->
	?log_warning("proto no match _Cmd:~p _R:~p",[_Cmd, _R]),
	{error, no_match}.


for(_Fun,_Body,0,Res)->
	lists:reverse(Res);
for(Fun,Body,Len,Res)->
	{Data,Remain}=Fun(Body),
	for(Fun,Remain,Len-1,[Data|Res]).

