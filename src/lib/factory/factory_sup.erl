-module(factory_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


%%--------------------------------------------------------------------
start_link(Config = {{WorkName,_,_},_,_}) ->
    supervisor:start_link({local,util:to_atom(util:to_list(WorkName) ++ util:to_list(?MODULE))}, ?MODULE, Config).

%% {{WorkName,WorkModule,WorkArg},Num,Mode}
init(Config = {{WorkName,_,_},_,_}) ->
	Factory = {factory, {factory,start_link,[Config]}, permanent, 2000, worker, [factory]},
    WorkerSup = {worker_sup, {worker_sup, start_link, [WorkName]}, permanent, 5000, supervisor, [worker_sup]},
    RestartStrategy = {one_for_one, 3, 100000},
	{ok, {RestartStrategy, [WorkerSup,Factory]}}.

