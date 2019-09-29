-module(world_app).

-include("common.hrl").

-export([start/2,stop/1]).

start(_StartType, _StartArgs) ->    
    {ok, Name} = application:get_env(name),
    {ok, Cookie} = application:get_env(cookie),
	net_kernel:start([Name, longnames]),
	util:sleep(1000),
    erlang:set_cookie(node(), Cookie),
	fun_gc:gc(),
    do_start().

stop(_State) ->
    ok.





do_start()-> 
	global:sync(),
	inets:start(),
	ssl:start(),
	case world_sup:start_link([]) of      
		{ok, Pid} -> {ok, Pid};
		Other ->     {error, Other}
	end.
