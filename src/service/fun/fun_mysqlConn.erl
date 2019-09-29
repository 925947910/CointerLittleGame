%% @author zh
%% @doc @todo Add description to fun_mysqlConn.


-module(fun_mysqlConn).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 do_call/2,
		 do_init/0,
		 do_msg/1,
		 do_info/1,
		 do_time/1,
		 do_close/0,
		 send_msg/1
		]).
-define(CleanCd,60).

do_init() ->
	init_mysql_conns().
init_mysql_conns()->
	
    {ok, Host} = application:get_env(dbhost),
	{ok, Port} = application:get_env(dbport),
	{ok, Usr} = application:get_env(dbusr),
	{ok, Psw} = application:get_env(dbpsw),
	{ok, Database} = application:get_env(dbdatabase),
	{ok, Code} = application:get_env(dbcode),
	?log(" connect db,DB=~p,Host=~p,Port=~p,Usr=~p,Database=~p,Code=~p,ConNUM=~p", [?DB,Host,Port,Usr,Database,Code,?SQLCONNUM]),
	mysql:start_link(?DB, Host,Port, Usr,Psw,Database),
	timer:sleep(2000),
	F = fun(_) -> 
				add_connect(?DB,Host,Port,Usr,Psw, Database,Code) 
		end,
	
	util:for(1,?SQLCONNUM - 1,F).

do_time(Time)->ok.

do_call(_,State) ->	
	{reply, nomatch, State}.



do_msg(Msg)-> ?log_warning("fun_mysqlConn msg Unmatch Msg:~p",[Msg]).


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

do_info(Info)-> ?log("fun_mysqlConn info Info:~p",[Info]).

do_close() -> ?log("fun_mysqlConn closed").

%%other use
send_msg(Msg) ->?send_world(mysqlConn,Msg).




add_connect(DB,Host, Port, Usr, Psw, Database,Code) ->
	?log("connect db,DB=~p,Host=~p,Port=~p,Usr=~p,Database=~p,Code=~p", [DB,Host,Port,Usr,Database,Code]),
	case mysql:connect(DB, Host, Port, Usr, Psw, Database,Code, true) of
		{?OK, _} -> ?OK;
		Other1 -> ?log_error("Error:mysql init,mysql:connect.Other=~p", [Other1])
	end.


%% 8. 连接 mysql
%% 首先进入Erlang Shell, 然后输入 mysql:start_link("连接名", "主机地址或IP", "mysql用户名", "mysql密码", "要访问的数据库").
%% e.g. mysql:start_link("test", "localhost", "root", "ifkirin1995", "mysql").
%% 
%% 
%% 9. 建立一个连接
%% mysql:connect("连接名", "主机地址或IP", undefined, "mysql用户名", "mysql密码", "要访问的数据库", true). 
%% e.g. mysql:connect("test", "localhost", undefined, "root", "ifkirin1995", "mysql", true). 
%% --------------------- 
%% 作者：ifkirin 
%% 来源：CSDN 
%% 原文：https://blog.csdn.net/ifkirin/article/details/52067486 
%% 版权声明：本文为博主原创文章，转载请附上博文链接！

