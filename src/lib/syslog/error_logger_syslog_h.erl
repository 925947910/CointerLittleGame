-module(error_logger_syslog_h).
-behaviour(gen_event).


%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).


-record(state, {}).


-include("common.hrl").

%%======================================
%% gen_event callback functions
%%======================================
init([])->
    {ok, #state{}}.


handle_event({error, _GLeader, {_PID, Format, Data}}, State) ->
	?log_error("=ERROR====~n"++Format, Data),
    {ok, State};
handle_event({info_msg, _GLeader, {_PID, Format, Data}}, State) ->
    ?log_error("=INFO====~n"++Format, Data),
    {ok, State};
handle_event({warning_msg, _GLeader, {_PID, Format, Data}}, State) ->
    ?log_error("=WARNING====~n"++Format, Data),
    {ok, State};
handle_event({error_report, _GLeader, {_PID, _Msg, Data}}, State) ->
	?log_error("=ERROR REPORT====~n~p", [Data]),
    {ok, State};
handle_event({info_report, _GLeader, {_PID, std_info, Data}}, State) ->
	?log_error("=INFO REPORT====~n~p", [Data]),
    {ok, State};
handle_event({warning_report, _GLeader, {_PID, _Msg, Data}}, State) ->
	?log_error("=WARNING REPORT====~n~p", [Data]),
    {ok, State};
handle_event(_Other, State)->
	{ok, State}.


handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

