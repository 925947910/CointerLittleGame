-module(fun_http).

-include("common.hrl").

-export([async_http_request/3, async_http_response/1,verify_cb/2]).

%% send_httpMsg_To_Web(TradeId,GroupId,RoomId,RedisPlayers)->
%% 	%%Data={"ver":Ver,"data":{"plays":[1,2,3,4],"groupId":1,"roomId":2},"session_id":"user_12069"},
%% 	%%?log("-----{TradeId,GroupId,RoomId,RedisPlayers}=~p",[{TradeId,GroupId,RoomId,RedisPlayers}]),
%% 	Url="http://192.168.1.207:8080/execute_player_event",
%% 	Ver=fun_redis:get_Web_Config(),
%% 	SessionId=util:to_binary("user_"++util:to_list(TradeId)),	
%% 	Obj={obj,[{"ver",Ver},{"session_id",SessionId}]},
%% 	Data=pt:fillobj(Obj, [{"data",[{obj,[{"plays",RedisPlayers},{"groupId",util:to_integer(GroupId)},{"roomId",RoomId}]}]}]), 
%% 	Data1="data="++rfc4627:encode(Data),
%%  	%%?log("-----Data1=~p",[Data1]),
%%  	async_http_request(post,{Url,[],"application/x-www-form-urlencoded",Data1}, {?MODULE, verify_cb,1}).
    
async_http_request(Method, Request, {Module, Cb_func, Cb_args}) ->
	case httpc:request(Method, Request, [], [{sync, false}, {receiver, self()}]) of
		{ok, Result} when erlang:is_reference(Result) ->
			case get(http_request_info) of
				L when erlang:is_list(L) ->
					RequestInfoList = L;
				_ ->
					RequestInfoList = []
			end,
			%%?log("-----get(http_request_info)=~p",[get(http_request_info)]),
			put(http_request_info, RequestInfoList ++ [{Result, {Module, Cb_func, Cb_args}}]),
			%%?log("http request,method=~p,request=~p", [Method, Request]),
			ok;
		{error, Reason} ->
			?log_warning("http request error, reason=~p", [Reason]),
			error;
		Other -> 
			?log("-----other"),
			?log_warning("unknown ret,value=~p", [Other]),
			error
	end.
		 
async_http_response(Response) ->
	case Response of
		{RequestId, {StatusLine, _Headers, Body}} ->
			case get(http_request_info) of
				RequestInfoList when erlang:is_list(RequestInfoList) ->
					case lists:keyfind(RequestId, 1, RequestInfoList) of
						{Key, {Module, Cb_func, Cb_args}} ->
							put(http_request_info, lists:keydelete(Key, 1, RequestInfoList)),
							Module:Cb_func({StatusLine, Body}, Cb_args);
						_ ->
							?log_warning("invalid http request,request_id=~p", [RequestId])
					end;
				_ ->
					?log_warning("no http RequestInfoList")
			end;
		Other ->
			?log_warning("unknown http response,response=~p", [Other])
	end,
	ok.
	
verify_cb({_StatusLine,_Body},_)->
%% 	io:format("!!!!!!verify_cb~p",[{_StatusLine,_Body}]),
ok.