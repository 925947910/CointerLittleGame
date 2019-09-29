-module(world_sup).
-behaviour(supervisor).
-export([start/1,start_link/1,init/1]).
-include("common.hrl").

start_link(_) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start(_) ->
    ?MODULE:start_link([]).

init([]) ->
	SvrList = [			   
                {sceneMng,  {world_svr,start_link,[sceneMng]},permanent, 5000, worker, [world_svr]},			   
			  	{sessionMng, {world_svr, start_link, [sessionMng]}, permanent, 5000, worker, [world_svr]},
%% 				{mysqlConn, {world_svr, start_link, [mysqlConn]}, permanent, 5000, worker, [world_svr]},
				{redisConn, {world_svr, start_link, [redisConn]}, permanent, 5000, worker, [world_svr]},
			    {shopMalln, {world_svr, start_link, [shopMall]}, permanent, 5000, worker, [world_svr]}
			
			  ],
    RestartStrategy = {one_for_one, 3, 10},
	?log("Info:public_sup init"),
	{ok, {RestartStrategy, SvrList}}.