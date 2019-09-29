-module(mynet_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start/0]).
-export([add/1]).
-export([relay/1]).

-include("common.hrl").
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    ?MODULE:start_link().

init([]) ->
    global:sync(),
    RestartStrategy = {simple_one_for_one, 10, 10},
    Children = {mynet_server, {mynet_svr, start_link, []}, temporary, 2000, worker, [mynet_svr]},
	{ok, Port} = application:get_env(port),	
    timer:apply_after(2000, ?MODULE, add, [Port]),
    {ok, {RestartStrategy, [Children]}}.


add(Port) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, Sock} ->
            relay(Sock);
        {error, Reason} ->
			?log("listen error Reason = ~p",[Reason]),
            {stop, {cannot_listen,Reason}}
    end.
relay(Sock) ->
%% 	?log("relay"),
    {ok, Pid} = supervisor:start_child(?MODULE, [add]),
    gen_tcp:controlling_process( Sock, Pid ),
    gen_server:cast(Pid, {accept, Sock}).

