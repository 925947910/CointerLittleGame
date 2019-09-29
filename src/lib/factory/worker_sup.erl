-module(worker_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-export([add/2]).

%%--------------------------------------------------------------------
start_link(WorkName) ->
    supervisor:start_link({local, util:to_atom(util:to_list(WorkName) ++ util:to_list(?MODULE))}, ?MODULE, {}).

init({}) ->
    Children = {worker, {worker, start_link, []}, permanent, 2000, worker, [worker]},
    RestartStrategy = {simple_one_for_one, 10, 10},
    {ok, {RestartStrategy, [Children]}}.

add(WorkName,Args) ->
    supervisor:start_child(util:to_atom(util:to_list(WorkName) ++ util:to_list(?MODULE)), [Args]).

