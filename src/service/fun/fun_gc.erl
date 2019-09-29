%% @author User
%% @doc @todo Add description to fun_gc.


-module(fun_gc).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([gc/0,hot_update/2,makeall/0,close_svr/1]).



makeall()->

	systools:make_script("mynet"),
	io:format("make mynet ok.~n"),
	systools:make_script("scene"),
	io:format("make scene ok.~n"),
	systools:make_script("world"),
	io:format("make world ok.~n"),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

gc()->
	[erlang:garbage_collect(Pid) || Pid <- processes()],
	 timer:apply_after(300000, ?MODULE, gc, []).





hot_update(Node,Mod)->
	?log_warning("======hot_updating=== datas:~p",[{Node,Mod}]),
	R1=rpc:call(Node, code, purge, [Mod], 5000),
	R2=rpc:call(Node, code, load_file, [Mod], 5000),
	io:format("hot_update*******************~p",[{R1,R2}]).
close_svr(Ip)->
	Node=util:to_atom("games_world@"++Ip),
	rpc:call(Node, fun_sessionMng, send_msg, [{close_svr}], 5000).	


%% 	   code:purge(Mod),      %% 清理模块（同时杀掉运行'old'代码的进程，'current'的不受影响）  
%%     code:load_file(Mod).  %% .