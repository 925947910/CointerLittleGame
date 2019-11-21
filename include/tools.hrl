%-define(log(F, D), syslog_svr:logs("[~p] " ++ F, [?MODULE|D])).
%-define(log(F), syslog_svr:logs("[~p] " ++ F, [?MODULE])).
%-define(mark(), syslog_svr:logs("MARK ~p : ~p ", [ ?MODULE, ?LINE ])). 
-define(mark(), syslog_svr:logs("[~10.10s] : ~p ", [ atom_to_list(?MODULE), ?LINE ])). 

-ifdef(robot).
-define(IFROBOT,true).
-else.
-define(IFROBOT,false).
-endif.

-ifdef(check_time).
-define(mark_time(Data),?log("mark_time now = ~p Data=~p",[util:unixtime(),Data])).
-define(mark(Data),?log("mark Data=~p",[Data])).
-else.
-define(mark_time(Data),ok).
-define(mark(Data),ok).
-endif.

-ifdef(secure).
-define(SECURE,true).
-else.
-define(SECURE,false).
-endif.




-ifdef(resort).
-define(RESORT,true).
-else.
-define(RESORT,false).
-endif.

-define(broadCast(Data), tool:broadCast(Data)).

-define(broadCast(Sid,Uid,Data), tool:broadCast(Sid,Uid,Data)).

-define(send(Sid, Data), tool:send_packet(Sid, Data)).
-define(sends(Sid, Data), tool:send_packets(Sid, Data)).
-define(send_world(Type,Msg), tool:send_to_world(Type, Msg)).
-define(discon(Sid,Reson,Time), timer:apply_after(Time, gen_server, cast, [Sid, {discon, Reson}])).


-define(jsonStr(A), (byte_size(util:to_binary(A))):32/integer, (util:to_binary(A))/binary).
-define(str(A), (byte_size(util:to_binary(A))):16/integer, (util:to_binary(A))/binary).
-define(u8,     8/unsigned-integer).
-define(u16,    16/unsigned-integer).
-define(u32,    32/unsigned-integer).
-define(i8,     8/signed-integer).
-define(i16,    16/signed-integer).
-define(i32,    32/signed-integer).
-define(f,      32/float).






