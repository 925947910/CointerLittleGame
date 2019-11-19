-include("proto.hrl").
-include("tools.hrl").
-include("td.hrl").
-include("db.hrl").
-include("log.hrl").
-include("record.hrl").
-include("code.hrl").
-include_lib("stdlib/include/qlc.hrl").


%%flash843
%%-define(FL_POLICY_REQ, <<"<polic">>).
-define(FL_POLICY_REQ, <<"<policy-file-request/>\0">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).
-define(FL_TGW_REQ, "tgw_l7_forward\r\nHost: vc3.app100693997.twsapp.com:8000\r\n\r\n").

%%tcp_server
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}, {delay_send, false}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).
-define(HEADER_LENGTH, 4).
-define(MINI_BODY, 2).
-define(MAX_BODY, 1024 * 1024).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).





-define(SCENE_TIMER_INTERVAL,500).

-define(TRUE,  true).
-define(FALSE,  false).

-define(Btrue, 1).
-define(Bfalse,0).



-define(OK, ok).
-define(ERROR, error).



-define(UNDEFINED, undefined).

-define(TcpOffLine, 0).
-define(TcpOnLine, 1).
-define(GameRegisted, 2).


-define(UNCONNECTED, 0).
-define(CONNECTED, 1).
-define(SHAKED, 2).
-define(USERCONNECTED, 3).
‰Œ


-define(HTTP_SVR_PORT,8889).
-define(HTTP_SVR_NAME,"GameHttpSvr").
-define(HTTP_SVR_CB_MODS,[game_http_rpc, io]).



-define(USER_DB,0).
-define(EVENT_DB,1).
-define(GAME_ITEM_DB,2).
-define(STATIC_DATA_DB,4).
-define(USER_STATUS_DB,5).


-define(EVENT_PAY,1).
-define(EVENT_WIN,2).
-define(EVENT_DIG_MINERAL,10).

-define(GAME_SNAKE,1).
-define(GAME_TANK,2).
-define(GAME_BOSSRUN,3).

-define(GAME_INIT,1).
-define(GAME_RUNING,2).
-define(GAME_OVER,3).


-define(CliEventDie,2).
-define(CliEventOffLine,3).
-define(CliEventBeam,4).
-define(CliEventWin,5).
-define(CliEventQuit,6).
-define(CliEventTankScore,8).
-define(CliEventBossRunOver,9).




