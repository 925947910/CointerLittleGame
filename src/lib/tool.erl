-module(tool).
-include("common.hrl").
-export([send_packet/2, send_packets/2, send_to_world/2,test/1, for/5,sys_msg/2,escape_uri/1,
		 broadCast/3,broadCast/1,unescape_string/1,checkstr/0,range/1,range/2,range/3,time/0,boolean2Num/1]).



	
checke_line_wall(X,Y)	->
		WallList=get(line_wall_list),
	
		checke_line_wall(X,Y,WallList).

checke_line_wall(_X,_Y,[]) -> false;
checke_line_wall(X,Y,[{ID,PointList}|Next]) ->
	case lists:member({X,Y}, PointList) of
		true -> 
			
			
			true;
		_ -> checke_line_wall(X,Y,Next)
	end;
checke_line_wall(_X,_Y,_Data) -> ?log("_Data = ~p",[_Data]),false.

checke_wall(X,Y)	->
		List=get(wall_list),
		if
			List==[]->false;
			true	->
				Fun=fun({ID,X1,Y1,H,W})	->			
						S1=boolean(X,X1,W),
							S2=boolean(Y, Y1, H),	
								Cond=S1==1andalso S2==1	,
					           if  
								   Cond==true->ok;
								   true->notdo
                                    
                               end,
					           Cond   
					
					end,
				lists:any(Fun, List)
		end.

boolean(A,A1,Dis)	->
	H = A-A1,
	if
		H > 0 orelse H == 0	->
			if
				H < Dis	-> 1;
				true -> 0
			end;
		 true -> 0
	end.

broadCast(#user_udp{ip=Host,port=Port,data=Data}) ->
	fun_udpServer:send_msg({send, Host, Port,Data}).
   
broadCast(Sid,Uid,Data) ->
    case is_pid(Sid) of
        true ->
%%             Len = byte_size(Data),
%% %%             Packet = << 0:16/integer, Len:16/integer, Data/binary >>,
%%                 case Data of  
%% 			        <<_:?u32, ?PT_SEND_CONTROL_BAG:?u16,_>>->skip;
%%                      _->timer:sleep(500)
%%                 end,
            gen_server:cast(Sid, {broadCast,Uid,Data});
        false ->
            ok
    end.
send_packet(Sid, Data) ->
    case is_pid(Sid) of
        true ->
%%             Len = byte_size(Data),
%% %%             Packet = << 0:16/integer, Len:16/integer, Data/binary >>,
%%                 case Data of  
%% 			        <<_:?u32, ?PT_SEND_CONTROL_BAG:?u16,_>>->skip;
%%                      _->timer:sleep(500)
%%                 end,
            gen_server:cast(Sid, {send, Data});
        false ->
            ok
    end.

send_packets(Sid, Datas) ->
    case is_pid(Sid) of
        true ->
            gen_server:cast(Sid, {sends, Datas});
        false ->
            ok
    end.

send_to_world(Key, Msg) -> 
%% 	?log("send_to_world:~p",[Key]),
						   gen_server:cast({global,Key}, Msg).




%% test(1) ->
%% 	db:dirty_select(usr, [{#usr{id='$1',name='$2',_='_'},[{'>','$1',30},{'<','$1',40},{'LIKE','$2',"111"}],['$_']}]);

test(3)	->  
%% 	A="~,@,#,$,&,*,_,`",
%% 	Content="adv@",
%% 	B=string:tokens(A, ","),
%% 	Fun=fun(C)->
%% 	string:str(C, Content) end,
%% 	lists:map(Fun, B);
fun_auction:send_msg({get_parm_1,1,2,0,1,1});


%% test(7)	->
%% 			case db:dirty_get(usr, 49)of
%% 				[#usr{name=Name}]->
%% 			case db:dirty_get(ply,util:to_binary(Name) , #ply.name) of
%% 						[#ply{sid=Sid}] when erlang:is_pid(Sid) ->ok;
%% 																		  	
%% 						_	->error
%% 			end;
%% 				_	->error
%% 			end;

test(8)	->
	put(test_points,[]),
	A=tool_vect:s2d_get_line_point({1,9}, {22,26}),
	Fun=fun({X1,Y1})	->				
				List=[{X1,Y1},{X1-1,Y1-1},{X1-1,Y1+1},{X1+1,Y1+1},{X1+1,Y1-1},{X1-1,Y1},{X1+1,Y1},{X1,Y1-1},{X1,Y1+1}],
				put_points(List) end,
	lists:foreach(Fun, A),
	N=get(test_points),
	?log("N=~p",[N]);
	
test(9)	->
	B=[{1,2},{2,6}],A=[{1,2},{2,6},{2,4}],
	F = fun(V) -> not lists:member(V, B) end,
	C=lists:filter(F , A),
	lists:append(B, C).

test(Point,Dis,Dir,Target) ->
	VD = tool_vect:format(tool_vect:s2d_get_vect_by_dir(tool_vect:angle2radian(Dir))),
	{Cx,Cy} = tool_vect:add(Point, tool_vect:ride(VD, Dis / 2)),
	{Cmx,Cmy} = tool_vect:add(Target,{-1 * Cx,-1 * Cy}),
	VL = tool_vect:s2d_get_vect_by_dir(tool_vect:angle2radian(Dir + 90)),
	W = tool_vect:s2d_dot_line_dis(VD,{Cmx,Cmy}),
	D = tool_vect:s2d_dot_line_dis(VL,{Cmx,Cmy}),
	{W,D}.

perms([]) -> [""];
perms(L)	-> io:format("L=~p~n",[L]),[[H|T]|| H<-L,T<-perms(L--[H])].


for(Max,Max,R,C,L)->[R(L)];
for(I,Max,R,C,L)->[R(L)|for(I+1,Max,R,C,C(L))].

sys_msg(Sid,Content) ->
	?log("sysmsg:~p" ,[Content]),
	ok.
%% 	{ok,Msg} = pt_game2c:write(?PT_CHAT, {"SYSTEM","ALLUSR",?CHANLE_SYSTEM,Content}),
%% 	?send(Sid,Msg).
%[{Type1,Num1,Impro1,Lev1},{Type2,Num2,Impro2,Lev2},{Type3,Num3,Impro3,Lev3},
%		 				{Type4,Num4,Impro4,Lev4},{Type5,Num5,Impro5,Lev5}]


list(L)	->L.


escape_uri(S) when is_list(S) ->
%%     escape_uri(unicode:characters_to_binary(S));
	escape_uri(list_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".
escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $A].

unescape_other(Str) ->
	unescape_other(Str, []).
unescape_other([], Acc) ->   
   	lists:reverse(Acc);
unescape_other([$%, H1, H2 |T], Acc) ->
    I1 =util:hex2int(H1),
    I2 =util:hex2int(H2),
    I = I1 * 16 + I2,
	unescape_other(T, [I, $\ |Acc]);
unescape_other([H|T], Acc) ->
    unescape_other(T, [H|Acc]).

unescape_string(Str) ->
	unescape_string(Str, []).
unescape_string([], Acc) ->   
   	lists:reverse(Acc);
unescape_string([$%, H1, H2 |T], Acc) ->
    I1 = util:hex2int(H1),
    I2 = util:hex2int(H2),
    I = I1 * 16 + I2,
	unescape_string(T, [I, []|Acc]);
unescape_string([H|T], Acc) ->
    unescape_string(T, [H|Acc]).
checkstr()	->
 		O = rfc4627:encode({obj, [{name, hideto}, {age, 23}]}),
		?log("O=~p",[O]).
range(Max) when Max >= 1->
	range(1, Max).
range(Min, Max) when Min>=1,Max>=Min->
	range(Min, Max, []).
range(Min, Min, L)->
	[Min|L];
range(_, 0, L)->
	L;
range(Min, Max, L)->
	range(Min, Max-1, [Max|L]).
time()	->
		{A,B}=erlang:localtime(),
		?log("A=~p",[A]).
put_points(List)	->
 	case get(test_points) of
			[]	->put(test_points,List);
 			Old_List	->
				F = fun(V) -> not lists:member(V, Old_List) end,
				C=lists:filter(F , List),
				N=lists:append(Old_List, C),
				put(test_points,N)
		end.
merge({X,Y})->				  				
		A=[{1,2},{2,3},{4,6}],
		lists:delete({X,Y}, A).

boolean2Num(Boolean)->
   case Boolean  of  
	   ?TRUE->?Btrue;
	   _->?Bfalse
   end.
