%%%-----------------------------------
%%% @Module  : util
%%% @Description: 
%%%-----------------------------------
-module(util).
-export([
        log/5,
		to_binary/1,
		f2s/1,
		to_integer/1,
		to_atom/1,
		list_to_atom2/1,
		to_list/1,
		show/5,
        unixtime/0,
        longunixtime/0,
		date/0,
		date/1,
        md5/1,
        rand/2,
		abs/1,
	ceil/1,
        floor/1,
        sleep/1,
        sleep/2,
        get_list/2,
        implode/2,
        implode/3,
        explode/2,
        explode/3,
        for/3,
        for/4,
        string_to_term/1,
        bitstring_to_term/1,
        term_to_string/1,
		unix_to_localtime/1,
		get_index_of/2,
        term_to_bitstring/1,
		map/2,
		get_bit/2,
		set_bit/3,
		escape_uri/1,
		unescape_string/1,
		unescape_other/1,
		timestamp/1,
		test_call/2,
		fun_call/2,
		key_max/2,
		key_min/2,
		gen_ip_list/1,
		gen_ip_list/2, 
		range/2,
		sample/2,
		get_relative_day/1,
		relative_day_to_unixtime/2,
		random_by_weight/1,
		bin_to_datas/3,
		datas_to_bin/3
    ]).

-include("common.hrl").

-define(SECONDS_FROM_0_TO_1970, 62167248000).
-define(SECONDS_PER_DAY, 86400).

%% List
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% ->
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 
log(T, F, A, Mod, Line) ->
    {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
    Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
	file:close(Fl).    
show(T, F, A, Mod, Line) ->
     Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(unicode:characters_to_list(Format), [Date, Mod, Line] ++ A).

%% unix
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

longunixtime() ->
    {M, S, Ms} = erlang:now(),
    M * 1000000000 + S*1000 + Ms div 1000.

date() -> unix_to_localtime(unixtime()).
date(Type)->
	{{Year,Mon,Day},{Hour,Min,Sec}}=unix_to_localtime(unixtime()),
	case Type of
		all ->
			(Year*10000000000)+(Mon*100000000)+(Day*1000000)+(Hour*10000)+(Min*100)+Sec;
		year->
			Year;
		yearmonday->
			Year*10000 + Mon*100 +Day;
		_->
			{{Year,Mon,Day},{Hour,Min,Sec}}
	end.
unix_to_localtime(UnixTime) when is_integer(UnixTime) ->
	MegaSecs = UnixTime div 1000000,
	Secs = UnixTime rem 1000000,
	calendar:now_to_local_time({MegaSecs, Secs, 0}).
timestamp(UnixTime) when is_integer(UnixTime)->
	{{Y,M,D},{H,Min,S}}=unix_to_localtime(UnixTime),
	integer_to_list(Y) ++ "-"  ++ integer_to_list(M) ++ "-" ++
	integer_to_list(D) ++ "  " ++ integer_to_list(H) ++ ":" ++ 
	integer_to_list(Min) ++ ":"  ++ integer_to_list(S).

get_relative_day(Hour) when is_integer(Hour)->
	{Day, {_Hour, _Min, _Sec}} = calendar:time_difference({{2010,1,1}, {Hour, 0, 0}}, calendar:local_time()),
	Day.

relative_day_to_unixtime(Day, Hour)  when is_integer(Day) and is_integer(Hour)->
	BaseTime = calendar:datetime_to_gregorian_seconds({{2010,1,1}, {Hour, 0, 0}}),
	Day * ?SECONDS_PER_DAY + BaseTime - ?SECONDS_FROM_0_TO_1970.

%% HEXmd5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


%%
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

abs(X) when X < 0 -> -1 * X;
abs(X) -> X.

 sleep(T) ->
    receive
    after T -> ok
    end.

 sleep(T, F) ->
    receive
    after T -> F()
    end.

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

%% for
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% for
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

%% termtermstringe.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% termtermbitstringe.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% termstringterme.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% termbitstringterme.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).


%%All To int
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> 
    round(Msg);
to_integer(_Msg) ->
    throw(other_value).
	

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) -> 
    Msg;
to_binary(Msg) when is_atom(Msg) ->
	list_to_binary(atom_to_list(Msg));
to_binary(Msg) when is_list(Msg) -> 
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> 
	list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) -> 
	list_to_binary(f2s(Msg));
to_binary(Msg) when is_tuple(Msg) ->
	list_to_binary(tuple_to_list(Msg));
to_binary(_Msg) ->
    throw(other_value).

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) -> 
	Msg;
to_atom(Msg) when is_binary(Msg) -> 
	util:list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) -> 
    util:list_to_atom2(Msg);
to_atom(_) -> 
    throw(other_value). 

list_to_atom2(List) when is_list(List) ->
	case catch(list_to_existing_atom(List)) of
		{'EXIT', _} -> erlang:list_to_atom(List);
		Atom when is_atom(Atom) -> Atom
	end.

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    f2s(Msg);
to_list(Msg) when is_tuple(Msg) ->
	tuple_to_list(Msg);
to_list(_) ->
    throw(other_value).


%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    A = io_lib:format("~.2f", [F]),
	A.

len_pos( X,[ H|T ] ) when X =:=H -> length(T)+1;
len_pos( X,[ H|T ] ) when X =/=H -> len_pos( X,T ).
	
get_index_of( X, List ) ->
	NewList = lists:reverse( List ),
	Index = len_pos( X,NewList ),
	Index.

-spec map(fun((D) -> [R]), [D]) -> [R].
map(F, [H|T]) ->
    lists:append([F(H),map(F, T)]);
map(F, []) when is_function(F, 1) -> [].

% start from 1...
get_bit(Int, Pos) ->
	Index = Pos - 1,
	case Int band (1 bsl Index) of
		0 -> 0;
		_ -> 1
	end.

% start from 1...
set_bit(Int, Pos, 1) ->
	Index = Pos - 1,
	Int bor (1 bsl Index);
set_bit(Int, Pos, 0) ->
	Index = Pos - 1,
	Int band (bnot (1 bsl Index)).

rand(Same, Same)-> Same;
rand(Min, Max) ->
    case get('rand_seed') of
        undefined ->
            RandSpeed=random:seed(erlang:now()),
            put('rand_seed', RandSpeed);
        _ ->skip
    end,
	if 
		Max > Min->	random:uniform(Max - (Min - 1)) + (Min - 1);
		Max < Min->	random:uniform(Min - (Max - 1)) + (Max - 1)
	end.

%% url_encode(Data) ->
%%     url_encode(Data,"").
%% 
%% url_encode([],Acc) ->
%%     Acc;
%% 
%% url_encode([{Key,Value}|R],"") ->
%%     url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
%% url_encode([{Key,Value}|R],Acc) ->
%%     url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).

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


%% Converts '%3c' to \<
unescape_other(Str) ->
	unescape_other(Str, []).
unescape_other([], Acc) ->   
   	lists:reverse(Acc);
unescape_other([$%, H1, H2 |T], Acc) ->
    I1 = hex2int(H1),
    I2 = hex2int(H2),
    I = I1 * 16 + I2,
	unescape_other(T, [I, $\ |Acc]);
unescape_other([H|T], Acc) ->
    unescape_other(T, [H|Acc]).

unescape_string(Str) ->
	unescape_string(Str, []).
unescape_string([], Acc) ->   
   	lists:reverse(Acc);
unescape_string([$%, H1, H2 |T], Acc) ->
    I1 = hex2int(H1),
    I2 = hex2int(H2),
    I = I1 * 16 + I2,
	unescape_string(T, [I, []|Acc]);
unescape_string([H|T], Acc) ->
    unescape_string(T, [H|Acc]).


fun_call(_Num,_N) -> ok.
test_call(_Num,_N)-> ok.

hex2int(H) when H >= $a -> 10 + H - $a;
hex2int(H) when H >= $A -> 10 + H -$A;
hex2int(H) ->  H - $0.

%% key_max([], KeyPos) -> Tuple | {}
key_max(List, KeyPos) ->
	key_max(List, KeyPos, {}).

key_max([], _KeyPos, Ret) ->
	Ret;
key_max([H|T], KeyPos, {}) ->
	key_max(T, KeyPos, H);
key_max([H|T], KeyPos, Ret) ->
	case element(KeyPos, H) > element(KeyPos, Ret) of
	?TRUE -> key_max(T, KeyPos, H);
	?FALSE -> key_max(T, KeyPos, Ret)
	end.

%% key_min([], KeyPos) -> Tuple | {}
key_min(List, KeyPos) ->
	key_min(List, KeyPos, {}).

key_min([], _KeyPos, Ret) ->
	Ret;
key_min([H|T], KeyPos, {}) ->
	key_min(T, KeyPos, H);
key_min([H|T], KeyPos, Ret) ->
	case element(KeyPos, H) < element(KeyPos, Ret) of
	?TRUE -> key_min(T, KeyPos, H);
	?FALSE -> key_min(T, KeyPos, Ret)
	end.


%% range(Min, Max) -> [Min,Min+1,Min+2,...,Max]
range(Min, Max) when Min>=0,Max>=Min->
	range(Min, Max, []).
range(Min, Min, L)->
	[Min|L];
range(_, 0, L)->
	L;
range(Min, Max, L)->
	range(Min, Max-1, [Max|L]).

%% gen_ip_list("1.2.3.4/30") -> IpList
gen_ip_list(CidrIp) when erlang:is_list(CidrIp)->
	[A,B,C,D,MaskBits] = string:tokens(CidrIp, "./"),
	gen_ip_list({list_to_integer(A), list_to_integer(B), list_to_integer(C), list_to_integer(D)}, list_to_integer(MaskBits)).
	
gen_ip_list(Ip, MaskBits) ->
	{A, B, C, D} = Ip,
	{M1, M2, M3, M4} = cidr_netmask(MaskBits),
	NetworkAddr = {A band M1, B band M2, C band M3, D band M4},
	BroadcastAddr = {A bor ((bnot M1) band 16#ff), B bor ((bnot M2) band 16#ff), C bor ((bnot M3) band 16#ff), D bor ((bnot M4) band 16#ff)},
	gen_ip_list_by_range(NetworkAddr,BroadcastAddr).
	
gen_ip_list_by_range(NetworkAddr, BroadcastAddr) ->
	{Na1, Na2, Na3, Na4} = NetworkAddr,
	{Ba1, Ba2, Ba3, Ba4} = BroadcastAddr,

	F3 = fun(V) ->
			lists:map(fun(_V)->erlang:list_to_tuple(V++[_V]) end, range(Na4, Ba4))
		end,
	
	F2 = fun(V) ->
			List = lists:map(fun(_V)->V++[_V] end, range(Na3, Ba3)),
			lists:map(F3, List)
		end,

	F1 = fun(V) ->
			List = lists:map(fun(_V)->[V,_V] end, range(Na2, Ba2)),
			lists:map(F2, List)
		 end,
	lists:flatten(lists:map(F1, range(Na1, Ba1))).
		
cidr_netmask(Bits) when is_integer(Bits) andalso Bits =< 32 ->
    ZeroBits = 8 - (Bits rem 8),
    Last = (16#ff bsr ZeroBits) bsl ZeroBits,
    
    case (Bits div 8) of
        0 ->
            {(255 band Last), 0, 0, 0};
        1 ->
            {255, (255 band Last), 0, 0};
        2 ->
            {255, 255, (255 band Last), 0};
        3 ->
            {255, 255, 255, (255 band Last)};
        4 ->
            {255, 255, 255, 255}
    end.

sample(List, N) when erlang:is_list(List) andalso erlang:is_integer(N) ->
	if 
		N < 0 orelse length(List) < N ->
			erlang:error(badarg);
		true ->
			sample(List, N, [])
	end.
sample(_List, N, RetList) when N =< 0 ->
	RetList;
sample(List, N, RetList) ->
  	Len = length(List),
	Index = random:uniform(Len),
	{L1, L2} = lists:split(Index, List),
	{L3, [Elem]} = lists:split(length(L1) - 1, L1),
	sample(L2++L3, N-1, [Elem|RetList]).

random_by_weight(List) ->
	F = fun({_Data, Weight}, Total) ->
			Total + Weight
		end,
	TotalWeight = lists:foldl(F, 0, List),
	random_by_weight(List, random:uniform() * TotalWeight, 0).

random_by_weight([{Data, Weight}|Next], R, Total) ->
	if 
		R =< Weight + Total ->
			Data;
		true ->
			random_by_weight(Next, R, Total + Weight)
	end;
random_by_weight([], _R, _Total) ->
	erlang:error(badarg).

	
	
bin_to_datas(<<>>,_Len,Res)->Res;
bin_to_datas(Bins,Len,Res)->
	case Bins of  
		<<Data:Len,Remain/bits>>->
			bin_to_datas(Remain,Len,[Data|Res]);
		_->[]
	end.
datas_to_bin([],_Len,Res)->Res;
datas_to_bin([Data|Remain],Len,Res)->
	datas_to_bin(Remain, Len, <<Data:Len,Res/bits>>).

