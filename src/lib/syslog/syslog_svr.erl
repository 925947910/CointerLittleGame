-module(syslog_svr).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_call/3 ]).
-export([logs_local/2, logs_local/1, 
		 log_trace/1, log_trace/2,
		 log_warning/1, log_warning/2,
		 log_error/1, log_error/2,
		 log_info/1,  log_info/2,
		 log_ue/1, log_ue/2, log_ue_format_str/2]).

-record(state, {host = "localhost", tport = 5410, uport = 514, tsock = 0, usock = 0, log_file=0}).

-include("common.hrl").

start_link({Host, Tport, Uport}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Tport, Uport], []).

stop() ->
    gen_server:cast(?MODULE, stop).
%%server
init([Host, Tport, Uport]) ->
   	{ok, Socket} = gen_udp:open(0, [binary]),
%%     timer:send_after(500, self(), timeout),
	error_logger:add_report_handler(error_logger_syslog_h),
	{ok, File} = file:open("syslog_svr.log", [append]),
   	{ok, #state{host = Host, tport = Tport, uport = Uport, usock = Socket, tsock = 0, log_file=File}}.

%%local					
%% init([Host, Tport, Uport]) ->
%% %%     {ok, Socket} = gen_udp:open(0, [binary]),
%% %%     timer:send_after(500, self(), timeout),
%% %%    {ok, #state{host = Host, tport = Tport, uport = Uport, usock = Socket, tsock = 0}}.
%% 	{ok, #state{host = Host, tport = Tport, uport = Uport, usock = 0, tsock = 0}}.

terminate(Reason, #state{log_file=File}) ->
	Format = "syslog_svr terminate,reason=~p",
	log2file(File, Format, [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_info({tcp_closed, Socket}, #state{tsock = Tsock} = State) ->
    case Socket == Tsock of
        true ->
            timer:send_after(1000, self(), timeout),
            {noreply, State#state{ tsock = 0 }};
        _ ->
            {noreply, State}
    end;

handle_info(timeout, #state{host = Host, tport = Port} = State) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}], 1000) of
        {ok, Socket} ->
            {noreply, State#state{tsock = Socket}};
        {error, _Reason} ->
            timer:send_after(1000, self(), timeout),
            {noreply, State}
    end;

handle_info(_Info, State) ->
	log2file(State#state.log_file, "Warning:syslog server,unknow msg. msg=~p", [_Info]),
	{noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

%% Priority=Facility * 8 + Level
%%
%% The list of Facilities available: 
%% 0 kernel messages 
%% 1 user-level messages 
%% 2 mail system 
%% 3 system daemons 
%% 4 security/authorization messages 
%% 5 messages generated internally by syslogd 
%% 6 line printer subsystem 
%% 7 network news subsystem 
%% 8 UUCP subsystem 
%% 9 clock daemon 
%% 10 security/authorization messages 
%% 11 FTP daemon 
%% 12 NTP subsystem 
%% 13 log audit 
%% 14 log alert 
%% 15 clock daemon 
%% 16 local use 0 (local0) 
%% 17 local use 1 (local1) 
%% 18 local use 2 (local2) 
%% 19 local use 3 (local3) 
%% 20 local use 4 (local4) 
%% 21 local use 5 (local5) 
%% 22 local use 6 (local6) 
%% 23 local use 7 (local7) 
%%
%% The list of severity Levels:
%% 0 Emergency: system is unusable 
%% 1 Alert: action must be taken immediately 
%% 2 Critical: critical conditions 
%% 3 Error: error conditions 
%% 4 Warning: warning conditions 
%% 5 Notice: normal but significant condition 
%% 6 Informational: informational messages 
%% 7 Debug: debug-level messages

handle_cast({log_error, Format, Data}, #state{host = Host, usock = Usock, uport = Uport} = State) ->
	try
	    Msg = io_lib:format("<139>" ++ Format ++ "~n",  Data),
		gen_udp:send(Usock, Host, Uport, Msg)
	catch
		E:W->log2file(State#state.log_file,"log error,E:W=~p,Line=~p,Format=~p,Data=~p",[{E,W}, ?LINE, Format, Data])
	end,
    {noreply, State};

handle_cast({log_warning, Format, Data}, #state{host = Host, usock = Usock, uport = Uport} = State) ->
	try
	    Msg = io_lib:format("<140>" ++ Format ++ "~n",  Data),
		gen_udp:send(Usock, Host, Uport, Msg)
	catch
		E:W->log2file(State#state.log_file,"log error,E:W=~p,Line=~p,Format=~p,Data=~p",[{E,W}, ?LINE, Format, Data])
	end,
    {noreply, State};

handle_cast({log_trace, Format, Data}, #state{host = Host, usock = Usock, uport = Uport} = State) ->
	try
	    Msg = io_lib:format("<141>" ++ Format ++ "~n",  Data),
		gen_udp:send(Usock, Host, Uport, Msg)
	catch
		E:W->log2file(State#state.log_file,"log error,E:W=~p,Line=~p,Format=~p,Data=~p",[{E,W}, ?LINE, Format, Data])
	end,
    {noreply, State};

handle_cast({log_info, Format, Data}, #state{host = Host, usock = Usock, uport = Uport} = State) ->
	try
	    Msg = io_lib:format("<142>" ++ Format ++ "~n",  Data),
		gen_udp:send(Usock, Host, Uport, Msg)
	catch
		E:W->log2file(State#state.log_file,"log error,E:W=~p,Line=~p,Format=~p,Data=~p",[{E,W}, ?LINE, Format, Data])
	end,
    {noreply, State};
handle_cast({log_ue, Format, Data}, #state{host = Host, usock = Usock, uport = Uport} = State) ->
	try
	    Msg = io_lib:format("<138>" ++ Format ++ "~n",  Data),
%		io:format(lists:flatten(io_lib:format("~s", [Msg]))),
		gen_udp:send(Usock, Host, Uport, Msg)
	catch
		E:W->log2file(State#state.log_file,"log error,E:W=~p,Line=~p,Format=~p,Data=~p",[{E,W}, ?LINE, Format, Data])
	end,
    {noreply, State};
%%local
handle_cast({logs_local, Format, Data}, State) ->
	try
	    Msg = io_lib:format("<142>" ++ Format ++ "~n", Data),
		{{Y,M,D},{H,Mi,S}} = erlang:localtime(),
		io:format("[~s-~2.2.0s-~2.2.0s ~2.2.0s:~2.2.0s:~2.2.0s]~s", [util:to_list(Y), util:to_list(M), util:to_list(D), util:to_list(H), util:to_list(Mi), util:to_list(S),Msg])
	catch
		E:W->io:format("log error,E:W=~p,Line=~p,Format=~p,Data=~p",[{E,W}, ?LINE, Format, Data])
	end,
    {noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

logs_local(Format, Data) when erlang:is_list(Format) andalso erlang:is_list(Data)->
    gen_server:cast(?MODULE, {logs_local, Format, Data}).
logs_local(Format) when erlang:is_list(Format)->
    logs_local(Format, []).

log_info(Format, Data) ->
	gen_server:cast(?MODULE, {log_info, Format, Data}).
log_info(Format) ->
    log_info(Format, []).

log_warning(Format, Data) ->
	gen_server:cast(?MODULE, {log_warning, Format, Data}).
log_warning(Format) ->
    log_warning(Format, []).

log_trace(Format, Data) ->
	gen_server:cast(?MODULE, {log_trace, Format, Data}).
log_trace(Format) ->
    log_trace(Format, []).

log_error(Format, Data) ->
	gen_server:cast(?MODULE, {log_error, Format, Data}).
log_error(Format) ->
    log_error(Format, []).

log_ue(Format, Data) ->
	gen_server:cast(?MODULE, {log_ue, Format, Data}).
log_ue(Format) ->
    log_ue(Format, []).
log_ue_format_str(Tag, KvList) ->
	JsObj = {obj, KvList},
	JsStr = rfc4627:encode(JsObj),
	io_lib:format("[~p]~s", [Tag, JsStr]).

log2file(File, Format, Data) ->
	Msg = io_lib:format(Format ++ "~n", Data),
	{{Y,M,D},{H,Mi,S}} = erlang:localtime(),
	case file:write(File, 
			io_lib:format("[~s-~2.2.0s-~2.2.0s ~2.2.0s:~2.2.0s:~2.2.0s]~s~n",
			[util:to_list(Y), util:to_list(M), util:to_list(D), util:to_list(H), util:to_list(Mi), util:to_list(S),Msg])) of
		{error, Reason} -> io:format("log2file error,reason=~p",[Reason]);
		_ -> skip
	end.

	
