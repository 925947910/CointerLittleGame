-module(scene_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->    
    {ok, Name} = application:get_env(name),
    {ok, Cookie} = application:get_env(cookie),
	{ok, World} = application:get_env(world),	
    net_kernel:start([Name, longnames]),
	util:sleep(1000),
    erlang:set_cookie(node(), Cookie),
	fun_gc:gc(),
    check_start(World).    

stop(_State) ->    
    ok.
check_start(World) ->
		?log("connect world...~p",[World]),
	case net_adm:ping(World) of
		pong -> 
			global:sync(),
			 do_start();
		_ -> util:sleep(2000),
			 check_start(World)
	end.

do_start()-> 
	global:sync(),
    inets:start(),
	ssl:start(),
    case scene_sup:start_link() of      
        {ok, Pid} ->      {ok, Pid};      
        Other ->          {error, Other}    
    end.

