%% @author User
%% @doc @todo Add description to fun_session.
-module(fun_session).
-include("common.hrl").

-export([session_msg/2,process_msg/2,do_time/1]).

session_msg(Sid,Msg)->
	gen_server:cast(Sid, {msg, Msg}).
process_msg(_,_)->?log("msg miss match").
do_time(_Now)->ok.





