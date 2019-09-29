-module(scene_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start/0]).
-export([add/1]).

%%--------------------------------------------------------------------
start_link() ->
	
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

start() ->
	
    ?MODULE:start_link([]).

init([]) ->
    Children = {scene, {scene_svr, start_link, []}, temporary, 2000, worker, [scene_svr]},
    RestartStrategy = {simple_one_for_one, 10, 10},
    {ok, {RestartStrategy, [Children]}}.
    
 
add(Params) ->
    supervisor:start_child({global,?MODULE}, [Params]).

