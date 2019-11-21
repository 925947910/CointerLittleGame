-module(fun_udpServer).
-include("common.hrl").
-define(PORT_UDP_OPTIONS, [binary, {active, true}]).
-define(GetSocket,get(socket)).
-define(PutSocket(Socket),put(socket,Socket)).
-export([do_init/0,do_close/0,do_call/1,do_info/1,do_time/1,do_msg/1,send_msg/1]).

do_init()-> 
	 {ok, Port} = application:get_env(udpPort),
	 {ok, Socket} = gen_udp:open(Port, ?PORT_UDP_OPTIONS),
	 ?PutSocket(Socket),
	 ok.
do_close() ->gen_udp:close(?GetSocket).

do_call(_Msg) -> ok.

do_info({udp, _SocketIn, Host, Port, Data}) ->
	fun_sessionMng:send_msg({sceneUdp,Host,Port,Data});

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

do_msg({send, Host, Port,Data}) ->
	gen_udp:send(?GetSocket, Host, Port, Data);

do_msg(Msg) ->
	?debug("unknown msg,msg=~p",[Msg]).

do_time(_Now) -> 1000.



send_msg(Msg) ->?send_world(udpServer,Msg).




	









