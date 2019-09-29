-module(pt).
-export([
            read_string/1,
			read_int/1,
			read_short/1,
			read_byte/1,
			read_sint/1,
			read_sshort/1,
			read_sbyte/1,
			read_bytes/1,
			add_string/2,
			add_int/3,
            pack/2
			
        ]).
-export([decrypt/1]).
-include("common.hrl").

%%
read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _R1 ->
                    {[],<<>>}
            end;
        _R1 ->
            {[],<<>>}
    end.

read_bytes(Bin)->
    case Bin of
        <<Len:32, Bin1/binary>> ->
            case Bin1 of				
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {Str, Rest};
                _R1 ->
                    {<<>>,<<>>}
            end;
        _R1 ->
            {<<>>,<<>>}
    end.


%%
read_int(Bin) ->
    case Bin of
        <<Data:32, Rest/binary>> ->
            {Data, Rest};
        _R1 ->
            {[],<<>>}
    end.
read_sint(Bin) ->
    case Bin of
        <<Data:32/signed-integer, Rest/binary>> ->
            {Data, Rest};
        _R1 ->
            {[],<<>>}
    end.

read_short(Bin) ->
	case Bin of
		<<Data:16, Rest/binary>> ->
			{Data, Rest};
		_R ->
			{[], <<>>}
	end.
read_sshort(Bin) ->
	case Bin of
		<<Data:16/signed-integer, Rest/binary>> ->
			{Data, Rest};
		_R ->
			{[], <<>>}
	end.
read_byte(Bin) ->
	case Bin of
		<<Data:8, Rest/binary>> ->
			{Data, Rest};
		_R ->
			{[], <<>>}
	end.

read_sbyte(Bin) ->
	case Bin of
		<<Data:8/signed-integer, Rest/binary>> ->
			{Data, Rest};
		_R ->
			{[], <<>>}
	end.

add_string(<<>>,Str) ->
	Bin = util:to_binary(Str),
    Len = byte_size(Bin),	
	<<Len:16, Bin/binary>>;
add_string(Arr,Str) ->
	Bin = util:to_binary(Str),
    Len = byte_size(Bin),	
	<<Arr/binary,Len:16, Bin/binary>>.

add_int(<<>>,Data,Len)->
	Bin = util:to_integer(Data),
	L = util:to_integer(Len),
	<<Bin:L>>;
add_int(Arr,Data,Len)->
	Bin = util:to_integer(Data),
	L = util:to_integer(Len),
	<<Arr/binary,Bin:L>>.
%% pack(Cmd, Data) ->
%%     L = byte_size(Data) + 6,
%%     <<L:?u32, Cmd:?u16, Data/binary>>.
pack(Cmd, Data) ->
    L = byte_size(Data) + 6,
	Head=encrypt(<<L:?u32>>),
	Body=encrypt(<<Cmd:?u16, Data/binary>>),
    <<Head/bits,Body/bits>>.

encrypt(Bin) ->
	case  Bin of  
		<<Data:1,Rest/bits>>->
			Len=bit_size(Rest),
			Cut=Len div 2,
			Cut2=(Len-Cut),
			<<D1:Cut,D2:Cut2>> = <<Rest/bits>>,
			<<Data:1,D2:Cut2,D1:Cut>>;
		_-><<>>
	end.

decrypt(Bin) ->
	case Bin of
		<<Data:1,Rest/bits>> ->
			Len=bit_size(Rest),
			Cut=(Len div 2)+1,
			Cut2=(Len-Cut),
			<<D1:Cut,D2:Cut2>> = <<Rest/bits>>,
			<<Data:1,D2:Cut2,D1:Cut>>;
		_-><<>>
	end.



