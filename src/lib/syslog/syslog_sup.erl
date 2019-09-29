-module(syslog_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start/0]).

%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    ?MODULE:start_link().

init([]) ->
    Children = {syslog_server, {syslog_svr, start_link, [{"localhost", 5140, 514}]}, permanent, 2000, worker, [syslog_svr]},
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, [Children]}}.

