-module(mynet_svr).
-behaviour(gen_server).
-export([start_link/1, stop/0, init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_call/3]).
-include("common.hrl").
-define(readySend,1).
-define(sended,0).
-record(state, {sock = 0, sid = 0 , conn_state=?UNCONNECTED, recv = 0, send=?sended, prev=0,close_reason=0,header=[]}).

start_link(_) ->
	gen_server:start_link(?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
	{ok, #state{sid = self()}}.



handle_info({tcp, Socket, Data}, #state{recv = Recv,conn_state=?CONNECTED} = State)->
	case Recv == 0 of
		true ->
			Packet = Data;
		_ ->
			Packet = list_to_binary([ Recv, Data ])
	end,
	case binary:matches(Packet, <<"\r\n\r\n">>) of
		[]->{noreply, State#state{recv=Packet}};
		_Find->
			HeaderList = string:tokens (util:to_list(Packet), "\r\n"),
			HeaderTupleList = [list_to_tuple (string:tokens (Header, ": ")) || Header <- HeaderList],
			SecWebSocketKey = proplists:get_value("Sec-WebSocket-Key", HeaderTupleList),
			case SecWebSocketKey  of
				?UNDEFINED->
					?discon(self(),noWebSocketKey,100),
					{noreply, State#state{header=HeaderTupleList}};
				_->
					Sha = crypto:hash (sha, [list_to_binary(SecWebSocketKey), <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
					Base64 = base64:encode (Sha),
					HandshakeHeader = [
									   <<"HTTP/1.1 101 Switching Protocols\r\n">>,
									   <<"Upgrade: websocket\r\n">>,
									   <<"Connection: Upgrade\r\n">>,
									   <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
									   <<"\r\n">>
									  ],
					?log("HandshakeHeader ok"),
					ok = gen_tcp:send(Socket, HandshakeHeader),
					{noreply, State#state{conn_state=?SHAKED,recv=0}}
			end
	end;
handle_info({tcp, _Socket, Data}, #state{recv = Recv,conn_state=ConnState} = State) when  ConnState==?SHAKED orelse ConnState==?USERCONNECTED ->
	case Recv == 0 of
		true ->
			Packet = Data;
		_ ->
			Packet = list_to_binary([ Recv, Data ])
	end,
	case ws_parse(State, Packet) of
		%% 		{ok, tgw} -> {noreply, State};
		%% 		{ok, policy} -> {stop, normal, State};
		{ok, Remain} -> {noreply, State#state{recv = Remain}};
		_ -> ?log("msg parse error")
	end;


handle_info({http, Msg}, State) ->
	fun_http:async_http_response(Msg),
	{noreply, State};
handle_info({timeout, _TimerRef, CallBackInfo}, State) ->
	case CallBackInfo of
		{Module, Function} ->
			apply(Module, Function, []);
		{Module, Function, Args} ->
			apply(Module, Function, Args);
		_ ->
			?log_warning("unknown timer callback,CallbackInfo=~p", [CallBackInfo])
	end,
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State#state{close_reason=tcp_closed}};

handle_info({tcp_error,_Socket,_}, State) ->
	{stop, normal, State#state{close_reason=tcp_error}};

handle_info(_Msg,State) ->{noreply, State}.

terminate(_Reason, State) ->
	close_tcp(State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({timercast}, #state{conn_state=Conn}=State) -> 
	Now = util:longunixtime(),    %%ms
	check_time_out(Now, Conn),
	fun_session:do_time(Now),
	timer:apply_after(1000, gen_server, cast, [self(), {timercast}]),
	{noreply, State};

handle_cast({msg,Msg},State) ->
	try
		fun_session:process_msg(State#state.conn_state, Msg)
	catch
		E:R:Stacktrace -> ?log_error("agent process_msg  error conn_state=~p,E=~p,R=~p Stack=~p",[State#state.conn_state,E,R,Stacktrace])
	end,
	{noreply, State};

handle_cast({recv,Recv,Len},State) ->
	   DataLen=Len-2,
	   case Recv   of  
		   <<Cmd:?u16,Data:DataLen/binary,_Remain/binary>> ->
           routing(Cmd,Data,State);
          _->?log_warning("illegal_proto"),skip
       end,
       {noreply, State};



handle_cast(flush,#state{sock=Socket,prev=Prev}=State)->
	if   erlang:is_list(Prev)->
			 Packet = list_to_binary(lists:reverse(Prev)),
			 gen_tcp:send(Socket, Packet);
		 true->skip
	end,	
	{noreply, State#state{send=?sended,prev = 0}};
handle_cast({sends, Datas}, State) ->
	Fun = fun(Elem, AccIn) ->
				  Len = byte_size(Elem),
				  Packet = << 0:16/integer, Len:16/integer, Elem/binary >>,
				  gen_server:cast(self(), {send, Packet}),
				  AccIn + 1
		  end,
	lists:foldl(Fun, 0, lists:reverse(Datas)),
	{noreply, State};

handle_cast({send, Data}, #state{send = Send, prev = Prev} = State) ->
	case Prev of
		0 ->
			P = [Data];
		_ ->
			P = [Data|Prev]
	end,
	case Send of
		?sended ->
			timer:apply_after(20, gen_server, cast, [self(), flush]);
		_ ->
			ok
	end,
	{noreply, State#state{prev = P, send = ?readySend}};
handle_cast({broadCast,Uid,Data}, #state{sock=Socket,send = Send, prev = Prev} = State) ->
	case get(uid) of  
		Uid->	case Prev of
					0 ->
						P = [Data];
					_ ->
						P = [Data|Prev]
				end,
				Packet = list_to_binary(lists:reverse(P)),
	            gen_tcp:send(Socket, Packet),
				{noreply, State#state{send=?sended,prev = 0}};
		_->{noreply, State}
	end;


handle_cast({setConnState, ConnState,Uid},State) ->
	put(uid,Uid),
	{noreply, State#state{ conn_state=ConnState}};

handle_cast({accept, Sock}, State) ->
 	  	?log("accept"),
	case gen_tcp:accept(Sock) of
		{ok, Socket} ->
			  			?log("accepted"),
			inet:setopts(Socket, [binary, {active, true}]),
			mynet_sup:relay(Sock),
			timer:apply_after(1000, gen_server, cast, [self(), {timercast}]),	
			put(conn_time,util:longunixtime()),
			{noreply, State#state{sock = Socket,conn_state = ?CONNECTED}};
		{error, Reason} ->
			?log("accept error, Reason = ~p ~n", [Reason]),
			mynet_sup:relay(Sock),
			{stop, accept_error, State}
	end;

handle_cast({discon, Reason}, #state{sid = Sid,sock = Sock} = State) ->
	?log("discon Sid=~p,id=~p ,reson=~p",[Sid,get(uid),Reason]),
	gen_tcp:close(Sock),
	{stop, normal, State#state{close_reason=Reason}};

handle_cast(Msg,#state{sid = Sid} =State) ->
	?log("mynet can not match Sid=~p,msg=~p",[Sid,Msg]),
	{noreply, State}.



close_tcp(#state{ sid = Sid,conn_state=Conn,close_reason=Reason}) ->
	fun_sessionMng:send_msg({tcp_close,Conn,get(uid),Sid,Reason}).

get_ip(Socket)->
	case inet:peername(Socket) of
		{ok,{{A,B,C,D},_Port}}->
			util:to_list(A)++"."++util:to_list(B)++"."++util:to_list(C)++"."++util:to_list(D);
		_-> "0.0.0.0"
	end.

routing(Cmd,DataBin,State) ->
	%% ?log("-----{ProtoNum,ReqId,JsonData,State}=~p",[{ProtoNum,ReqId,JsonData,State}]),
	%% ?log("-----get_ip(State#state.sock)=~p",[get_ip(State#state.sock)]),
	RunParams = try
					  pt_reader:read(Cmd, DataBin)
				catch E1:R1:Stacktrace1 -> ?log_error(" client msg routing error E=~p,R=~p,Call=~p",[E1,R1,Stacktrace1])
				end,
	case RunParams of
		{ok,Module,Act,Data} ->
			try
				Module:handle({},Act,Data,get_ip(State#state.sock))
			catch E:R:Stacktrace -> ?log_error("  client msg execute error Module=~p,Fun=~p,E=~p,R=~p,Call=~p",[Module,Act,E,R,Stacktrace])
			end;
		Other ->?log("client msg illegal ~p",[Other])
				%% 				?discon(self(),'client msg illegal',400)
	end.

%% routing(ProtoNum,Binary,State) ->
%% 	RunParams = try
%% 					pt_reader:read(ProtoNum, Binary)
%% 				catch E1:R1 -> ?log_error(" client msg routing error E=~p,R=~p,Call=~p",[E1,R1,erlang:get_stacktrace()])
%% 				end,
%% 	case RunParams of
%% 		{ok,Module, Fun, Data} ->
%% 			try
%% 				Module:handle(Fun,Data,get_ip(State#state.sock))
%% 			catch E:R -> ?log_error("  client msg execute error Module=~p,Fun=~p,E=~p,R=~p,Call=~p",[Module,Fun,E,R,erlang:get_stacktrace()])
%% 			end;
%% 		Other ->?log("client msg illegal ~p",[Other])
%% 				%% 				?discon(self(),'client msg illegal',400)
%% 	end.


check_time_out(Now,Conn)->
	case get(conn_time) of
		ConnTime when erlang:is_integer(ConnTime)->
			if  
				Now>ConnTime+30000 andalso Conn=/=?USERCONNECTED->
					?discon(self(),conn_time_out,1);
				true->skip
			end;
		_->skip
	end.


parse(#state{sock = Socket,sid = Sid} = State, Packet) ->
%% 	  	?log("Packet=~p",[Packet]),
	case Packet of
		%%flash policy
		?FL_POLICY_REQ ->
			gen_tcp:send(Socket, ?FL_POLICY_FILE),
			{ok, policy};
		<<Len:?u32, Remain/binary >> ->
			BodyLen = Len - ?HEADER_LENGTH,
			if BodyLen < ?MINI_BODY ->?discon(Sid,'msg error minibody',400),{ok, 0};
				BodyLen > ?MAX_BODY  -> ?discon(Sid,'msg error maxbody',400),{ok, 0};
				true ->
					case Remain of
						<<Data:BodyLen/binary, Remain2/binary >> ->
							case Data of
								<<?PT_PING:?u16,_Data/binary>>->
									put(outTimes,util:unixtime()+20),
									{ok, Bin} = pt_writer:write(?PT_PING,0),
									?send(Sid, Bin);
								_ ->
									gen_server:cast(self(), {recv,Data})
							end,
							case byte_size(Remain2) > 0 of
								true ->
									parse(State, Remain2);
								_ ->
									{ok, 0}
							end;
						_ ->
							{ok, Packet}
					end
			end;
		_ -> {ok, Packet}
	end.
ws_parse(#state{sock = Socket,sid = Sid} = State, Packet) ->
	Data=case Packet of
			 << B1:1,B2:3,B3:4,B4:1,Rest/bits >> when B1=/=1 orelse B2=/=0 orelse B3=/=2 orelse B4=/=1->
				 {failed,'illegal_bin'};
			 << 1:1, 0:3, 2:4, 1:1, Len:7, MaskKey:32, Rest/bits >> when Len < 126 ->
				 {Len,MaskKey,Rest};
			 << 1:1, 0:3, 2:4, 1:1, 126:7,Len:16, MaskKey:32, Rest/bits >>->
				 {Len,MaskKey,Rest};
			 << 1:1, 0:3, 2:4, 1:1, 127:7,Len:64, MaskKey:32, Rest/bits >> ->
				 {Len,MaskKey,Rest};
			 _-> Packet
		 end,
	case Data  of  
		Packet->{ok,Packet};
		{failed,Reason}->?discon(Sid,Reason,400),{ok, 0};
		{BodyLen,Mask,BinData}->
			if 
				BodyLen < ?MINI_BODY -> ?discon(Sid,'minibody',400),{ok, 0};
				BodyLen > ?MAX_BODY  -> ?discon(Sid,'maxbody',400),{ok, 0};
				true ->
					case BinData of
						<<CurrData:BodyLen/binary, Remain/binary >> ->
							case unmask(CurrData, Mask, <<>>) of
								<<?PT_PING:?u16,_Data/binary>>->
									put(outTimes,util:unixtime()+20),
									{ok, Bin} = pt_writer:write(?PT_PING,0),
									?send(Sid, Bin);
								 ProtoData->
									 gen_server:cast(self(),{recv,ProtoData,BodyLen})
							end,
							case byte_size(Remain) > 0 of
								true ->
									ws_parse(State, Remain);
								_ ->{ok, 0}
							end;
						_ ->{ok, Packet}
					end
			end
	end. 
		
unmask(<<>>, _, Unmasked) ->
	Unmasked;
unmask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	unmask(Rest, MaskKey, << Acc/binary, T:32 >>);
unmask(<< O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:24 >>;
unmask(<< O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:16 >>;
unmask(<< O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:8 >>.

