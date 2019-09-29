-module(db).
-include("common.hrl").

-export([start/0, stop/0, load_table/1,load_tables/1, truncate/1,do/1, find/3]).
-export([delete/2,getlocal/2,getnet/2,getnet/3,put/1,clear/1]).
-export([insert/1, insert/2,insert_whole/1]).
-export([insert_sql/2,get_sql_new_id/1]).
-export([load_all/1]).
-export([get_config/1,get_all_config/1,set_config/2,set_all_config/2]).
-export([pick_all_sql_data/0,get_sql_data/1,init_sql_data/0,insert_sql_data/1,delete_sql_data/1]).
-export([mysql_run/1,get_sql_config/1]).
-export([tran_get/2,tran_get/3,dirty_get/2,dirty_get/3,tran_put/1,dirty_put/1,tran_match/2,dirty_match/2,tran_del/2,dirty_del/2
		,dirty_select/2]).
-export([get_db_load_col_name/1,get_db_load_col_name/2,get_db_load_all_name/1,get_db_load_name/1,get_db_save_name/1,get_db_insert_name/1,get_db_insert_no_id_name/1,get_db_truncate_name/1]).
-export([getOrFindData/4,getOrKeyFindData/2,getOrLoadData/4,getOrKeyLoadData/2]).
-export([init_ets_table/2,get_ets_data/2,put_ets_data/2,del_ets_data/2,match_ets_data/2,select_ets_data/2]).

-export([put_obj_datas/3,get_obj_datas/2,get_class_objs/1,filter_class_objs/2,del_class_objs/1,del_obj_datas/2]).
get_db_load_col_name(Tab) -> util:to_atom("load_col_" ++ util:to_list(Tab)).
get_db_load_col_name(Tab,Col) when is_atom(Col) -> util:to_atom("load_col_" ++ util:to_list(Tab) ++ "_" ++ util:to_list(Col)).
get_db_load_all_name(Tab) -> util:to_atom("load_all_" ++ util:to_list(Tab)).
get_db_load_name(Tab) -> util:to_atom("load_" ++ util:to_list(Tab)).
get_db_truncate_name(Tab) -> util:to_atom("truncate_" ++ util:to_list(Tab)).
get_db_save_name(Tab) -> util:to_atom("save_" ++ util:to_list(Tab)).
get_db_insert_name(Tab) -> util:to_atom("insert_" ++ util:to_list(Tab)).
get_db_insert_no_id_name(Tab) -> util:to_atom("insert_no_id_" ++ util:to_list(Tab)).

start(Node) ->
    mnesia:delete_schema([node()]),
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [Node]).

start() ->
	loop_start().

loop_start() ->
	case catch gen:call({global, dbm}, '$gen_call', where) of
		{ok,Data} ->
			case Data of
				{ok,Node} -> start(Node),ok;
				_ -> ?log_trace("db:start() dbm initing start again"),
					 util:sleep(100),
					 loop_start()
			end;
		{'EXIT',Reason} ->
			?log_trace("db:start() exit!Reason=~p",[Reason]),
	    	error
	 end.

init_sql_data() ->
	?log("sql data init"),
	ets:new(sql_data, [set,public,named_table,{keypos, #sql_data.key}]).

insert_sql_data(Data) ->
	ets:insert(sql_data, Data).

delete_sql_data(Key) ->
	ets:delete(sql_data, Key).

get_sql_data(Key) ->
%% 	?log("get_sql_data ~p",[Key]),
	case ets:lookup(sql_data, Key) of
        [Data] -> Data;
		[] -> null;
        Other -> ?log_error("Error:get sql_data error,key=~p,Other=~p",[Key,Other]),?UNDEFINED
    end.


pick_all_sql_data() ->
	List = ets:tab2list(sql_data),
	ets:delete_all_objects(sql_data),
	{List1,List2,List3} = pick_data(List,[],[],[]),
	{List1,List2,List3}.

pick_data([],Data1,Data2,Data3) -> {Data1,Data2,Data3};
pick_data([This = #sql_data{opType =  del} | Next],Data1,Data2,Data3) -> pick_data(Next,Data1 ++ [This],Data2,Data3);
pick_data([This = #sql_data{opType =  add} | Next],Data1,Data2,Data3) -> pick_data(Next,Data1,Data2 ++ [This],Data3);
pick_data([This = #sql_data{opType =  change} | Next],Data1,Data2,Data3) -> pick_data(Next,Data1,Data2,Data3 ++ [This]);
pick_data([_ | Next],Data1,Data2,Data3) -> pick_data(Next,Data1,Data2,Data3).


%% check_sql_id(Tab,Id)-> 
%% 	case db:get_ets_data(sync_ids, {Tab,Id})  of 
%% 		[#sync_ids{key=Key}]->
%% 			db:del_ets_data(Tab, Key),
%% 			add;
%% 		_->case mnesia:dirty_read({sql_key_static, Tab}) of
%% 			   [Sql_key] -> if
%% 								Id > Sql_key#sql_key_static.max -> add;
%% 								true -> change
%% 							end;
%% 			   _Other -> ?debug("_Other=~p,Key=~p",[_Other,Tab]),change
%% 		   end
%% 	end.

get_sql_new_id(Key) ->
	case catch gen:call({global, key}, '$gen_call', {getkey,Key},infinity) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    ?log_error("get_sql_new_id call error Key=~p,Reason=~p",[Key,Reason]),fail
    end.

get_sql_config(Key) ->
	case mnesia:dirty_read(sql_config,Key) of
        [Config] -> Config#sql_config.sql;
        _ -> ?UNDEFINED
    end.



%%config
get_all_config(Key) ->
	case db:dirty_get(config, Key) of
        [Config] -> Config#config.value;
         Other -> ?log_error("Error:get all config error,key=~p,Other=~p",[Key,Other]),?UNDEFINED
    end.
get_config(Key) ->
	case db:dirty_get(config, {node(),Key}) of
        [Config] -> Config#config.value;
         Other -> ?log_error("Error:get  config error,key=~p,Other=~p",[{node(),Key},Other]),?UNDEFINED
    end.


set_config(Key,Val) ->
	db:tran_put(#config{key = {node(),Key},value = Val}).
set_all_config(Key,Val) ->
	db:tran_put(#config{key = Key,value = Val}).




insert(Rec) ->
	dbm_worker:work({insert, erlang:element(1, Rec), Rec}).
insert(Tab, Rec) ->
	dbm_worker:work({insert, Tab, Rec}).
insert_whole(Rec) ->
	Tab=erlang:element(1, Rec),
	Key=erlang:element(2, Rec),
	gen_server:call({global, dbm}, {sync_id,Tab,Key}),
	db:tran_put(Rec),
    gen_server:call({global, key}, {setkey,Tab,Key}).
load_all(Tab) ->
	dbm_worker:work({loadall, Tab}).

insert_sql(Tab, Rec) ->
	dbm_worker:work({insert_sql, Tab, Rec}).

stop() ->
    mnesia:stop().

truncate(Tab) ->
	dbm_worker:work({truncate, Tab}).

load_table(Table) ->
    case mnesia:add_table_copy(Table, node(), ram_copies) of
		{aborted, Reason} -> ?log_warning("mnesia add_table_copy failed,reason=~p",[Reason]);
		_ -> skip
	end,
    case mnesia:wait_for_tables([Table], 10000) of
		{timeout, TabList} ->
			?log_warning("mnesia wait_for_tables timeout,tables=~p",[TabList]);
		_ -> skip
	end.

load_tables(Tables) ->
	Fun = fun(Tab) -> load_table(Tab) end,
	lists:foreach(Fun, Tables).

find(Tab, Field, Val) ->
	dbm_worker:work({find, Tab, Field, Val}).


delete(Table,Key)->
	 case mnesia:transaction(fun() -> mnesia:delete({Table, Key}) end) of
        {atomic, _} -> ok;
        _ -> []
    end.


getlocal(Table,Key)when erlang:is_integer(Key)->
	case mnesia:transaction(fun() -> mnesia:read({Table, Key}) end) of
        {atomic, Result} -> Result;
        _ -> []
    end;	
getlocal(Table,Pat) ->
	case mnesia:transaction(fun() -> mnesia:match_object(Table, Pat, read) end) of
        {atomic, E} -> E;
        _ -> []
	end.
getnet(Table,Key)->
	dbm_worker:work( {load, Table, Key}).
getnet(Table,KeyName,KeyVal) when is_atom(KeyName) ->
	dbm_worker:work({load, Table, KeyName,KeyVal}).

put(Record) ->	tran_put(Record).


mysql_run({update,Tab,Rec})->
%% 	?debug("Rec = ~p",[Rec]),
	[Tab| Datas] = util:to_list(Rec),
	[Id | NextData] = Datas,
	case mysql:execute(?DB,db:get_db_save_name(Tab),NextData ++ [Id]) of
		{updated,{_,_,_,Lines,_}} when Lines =:= 0 ->[];
		{updated,{_,_,_,_Lines,_}} ->[Rec];
		Other -> ?log_error("update fail Other=~p,Tab =~p",[Other,Tab]),error
	end;

%%..... rechange
mysql_run({run,Sql})->
	case mysql:fetch(?DB_S,Sql) of
		{updated,{_,_,_,_Lines,_}} -> ?OK;
		{data,{mysql_result,[_|_],As,_, _}}->{?OK,As};
		Other -> ?log_error("run fail Sql=~p,Other=~p",[Sql,Other]),error
    end;

mysql_run({load,Tab,Id})->
	load(?DB, Tab, db:get_db_load_name(Tab),[Id]);

mysql_run({load,Tab, Field, Val})->
	PrePareName = db:get_db_load_col_name(Tab,Field),
	case mysql:get_prepared(PrePareName) of
		{ok,_} -> skip;
		_ -> 
			case get_fields(Tab) of
				?UNDEFINED -> ?log_error("undefined update tab ~p", [Tab]),[];
				Fields ->
					Sql = "SELECT " ++ Fields ++ " FROM " ++ util:to_list(Tab) ++ " WHERE " ++ util:to_list(Field) ++ " = ?",
					mysql:prepare(PrePareName, util:to_binary(Sql))
			end
	end,
	load(?DB, Tab, PrePareName,[Val]);

mysql_run({loadall,Tab})->
	load(?DB, Tab, db:get_db_load_all_name(Tab),[]);

mysql_run({find,Tab, Field, Val})->
	PrePareName = db:get_db_load_col_name(Tab,Field),
	case mysql:get_prepared(PrePareName) of
		{ok,_} -> skip;
		_ -> 
			case get_fields(Tab) of
				?UNDEFINED -> ?log_error("undefined update tab ~p", [Tab]),[];
				Fields ->
					Sql = "SELECT " ++ Fields ++ " FROM " ++ util:to_list(Tab) ++ " WHERE " ++ util:to_list(Field) ++ " = ?",
					mysql:prepare(PrePareName, util:to_binary(Sql))
			end
	end,
	find(?DB, Tab, PrePareName,[Val]);

mysql_run({truncate,Tab})->
	case get_fields(Tab) of
		?UNDEFINED -> ?log_error("undefined update tab ~p", [Tab]),[];
		_ -> Sql=list_to_binary("TRUNCATE TABLE " ++ atom_to_list(Tab)),
			 case mysql:fetch(?DB, Sql) of
				{error,{_,_,_,_,Reason}} -> ?log_error("truncate error Tab=~p,Sql=~p,Reason=~p",[Tab,Sql,Reason]),error;
				{error,Reason} -> ?log_error("truncate fail Tab=~p,Sql=~p,Reason=~p",[Tab,Sql,Reason]),error;
				_ -> ok
			 end
	end;

mysql_run({insert_sql,Tab, Rec})->
	[Tab|Datas] = util:to_list(Rec),
	case mysql:execute(?DB,db:get_db_insert_name(Tab),Datas) of
		{updated,{_,_,_,Lines,Re}} when Lines =:= 0 -> ?log_error("insert_sql fail Tab =~p,Re=~p",[Tab,Re]),[];
		{updated,{_,_,_,_Lines,_}} ->[Rec];
		Other -> ?log_error("insert_sql fail Other=~p,Tab =~p",[Other,Tab]),error
    end;

mysql_run({insert,_Tab, Rec})->
%% 	?debug("insert Rec=~p",[Rec]),
	case get_insert_id(Rec) of
		fail -> ?log_error("insert tab fail Rec=~p", [Rec]),0;
		NewId ->
%% 			?debug("NewId = ~p",[NewId]),
			New = erlang:setelement(2, Rec, NewId),
%% 			?debug("New = ~p",[New]),
%% 			dbm_worker:new_task(#sql_data{key={Tab,NewId},opType=add,rec=New}),
			db:tran_put(New),
			NewId
%% 			case mnesia:transaction(fun() -> mnesia:write(New) end) of
%% 				{atomic,ok} -> {NewId,New};
%% 				Other -> ?log("Other =~p",[Other]),{0,[]}
%% 			end			
	end;

mysql_run({delete,Tab, Key})->
	case get_fields(Tab) of
		?UNDEFINED -> ?log_error("undefined update tab ~p", [Tab]),[];
		_ -> 	Sql = list_to_binary(lists:concat([ "DELETE FROM ", Tab, " WHERE id = ", Key ])),
				case mysql:fetch(?DB, Sql) of
					{error,{_,_,_,_,Reason}} -> ?log_error("delete error, reason:~p , sql:~p", [Reason,Sql]), error;
					{error, Reason} -> ?log_error("delete fail, reason:~p , sql:~p", [Reason,Sql]), error;
					_ -> ok
				end
	end;

mysql_run(R) ->
 	?log_error("mysql_run R=~p",[R]),
	ok.

find(Con, Tab, PrePare, Args) ->
%% 	?debug("Args = ~p",[Args]),
    case mysql:execute(Con, PrePare, Args) of
        {data,{mysql_result,[_|_],As,_, _}} ->
            Fun =   fun(Elem, AccIn) ->
                        Rec = list_to_tuple([Tab|Elem]),
                        [Rec | AccIn]
                    end,
            lists:foldl(Fun, [], As);
		{error,{_,_,_,_,Reason}}->
			?log_error("find error Con=~p,Tab=~p,PrePare=~p,Reason=~p",[Con,Tab,PrePare,Reason]),[];
        Res ->
			?log_error("find fail Con=~p,Tab=~p,PrePare=~p,Res=~p",[Con,Tab,PrePare,Res]),[]
    end.

load(Con, Tab, PrePare, Args) ->
	load_tran(Con, Tab, PrePare, Args).

%% load_dirty(Con, Tab, PrePare, Args) -> 
%% 	case mysql:execute(Con, PrePare, Args) of
%%         {data,{mysql_result,[_|_],As,_, _}} ->
%%             Fun =   fun(Elem, AccIn) ->
%%                         Rec = list_to_tuple([Tab|Elem]),
%% 						db:dirty_put(Rec),
%%                         [Rec | AccIn]
%%                     end,
%% 			lists:foldl(Fun, [], As);
%% 		{error,{_,_,_,_,Reason}}->
%% 			?log_error("load fail Con=~p,Tab=~p,PrePare=~p,Reason=~p",[Con,Tab,PrePare,Reason]),[];
%%         Res ->
%% 			?log_error("load error Con=~p,Tab=~p,PrePare=~p,Res=~p",[Con,Tab,PrePare,Res]),[]
%%     end.

load_tran(Con, Tab, PrePare, Args) -> 
	case mysql:execute(Con, PrePare, Args) of
        {data,{mysql_result,[_|_],As,_, _}} ->
            Fun =   fun(Elem, AccIn) ->
                        Rec = list_to_tuple([Tab|Elem]),
						mnesia:write(Rec),
                        [Rec | AccIn]
                    end,
			case mnesia:transaction(fun() -> lists:foldl(Fun, [], As) end) of
            	{atomic, Recs} -> Recs;
				Other -> ?log_error("load error Con=~p,Tab=~p,PrePare=~p,Other=~p",[Con,Tab,PrePare,Other]),[]
			end;
		{error,{_,_,_,_,Reason}}->
			?log_error("load fail Con=~p,Tab=~p,PrePare=~p,Reason=~p",[Con,Tab,PrePare,Reason]),[];
        Res ->
			?log_error("load error Con=~p,Tab=~p,PrePare=~p,Res=~p",[Con,Tab,PrePare,Res]),[]
    end.

get_fields(Tab) -> db:get_sql_config(Tab).

%% get_save_sql(Rec) ->
%% 	[Tab|Data1] = util:to_list(Rec),
%% 	[Id|Data] = Data1,
%% 	case db:get_sql_config({Tab,save}) of
%% 		?UNDEFINED -> ?UNDEFINED;
%% 		Sql -> 
%% 			Str = io_lib:format(Sql, lists:append(Data ,[Id] )),
%%     		lists:flatten(Str)
%% 	end.
%% 
%% 
%% get_load_sql(Tab ) ->
%% 	db:get_sql_config({Tab,load}).
%% 
get_insert_id(Rec ) ->
	Tab = erlang:element(1, Rec),
	get_sql_new_id(Tab).

%% get_insert_sql(Rec) ->
%% 	[Tab|Data1] = util:to_list(Rec),
%% 	[_Id|Data] = Data1,
%% 	
%% 	case db:get_sql_config({Tab,insert_re_id}) of
%% 		?UNDEFINED ->?UNDEFINED;
%% 		Sql -> 
%% 			Str = io_lib:format(Sql, Data),
%%     		lists:flatten(Str)
%% 	end.
%% 
%% get_insert_sql_add_id(Rec) ->
%% 	[Tab|Data] = util:to_list(Rec),
%% 
%% 	case db:get_sql_config({Tab,insert}) of
%% 		?UNDEFINED ->?UNDEFINED;
%% 		Sql -> 
%% 			Str = io_lib:format(Sql, Data),
%%     		lists:flatten(Str)
%% 	end.


tran_get(Tab,Key) -> 
	case mnesia:transaction(fun() -> mnesia:read(Tab, Key) end) of
		{atomic, E} -> E;
		Other -> ?log_error("tran_get error Tab=~p,Key=~p,Other=~p",[Tab,Key,Other]),[]
	end.
tran_get(Tab,Key,KeyPos) -> 
	case mnesia:transaction(fun() -> mnesia:index_read(Tab, Key, KeyPos) end) of
		{atomic, E} -> E;
		Other -> ?log_error("tran_get error Tab=~p,Key=~p,KeyPos=~p,Other=~p",[Tab,Key,KeyPos,Other]),[]
	end.
dirty_get(Tab,Key) ->
	case mnesia:dirty_read(Tab, Key) of
		L when erlang:is_list(L) -> L;
		Other -> ?log_error("dirty_get error Tab=~p,Key=~p,Other=~p",[Tab,Key,Other]),[]
	end.
				 
dirty_get(Tab,Key,KeyPos) ->
	case mnesia:dirty_index_read(Tab, Key, KeyPos) of
		L when erlang:is_list(L) -> L;
		Other -> ?log_error("dirty_get error Tab=~p,Key=~p,KeyPos=~p,Other=~p",[Tab,Key,KeyPos,Other]),[]
	end.

tran_put(Rec) ->
	case mnesia:transaction(fun() -> mnesia:write(Rec) end) of
		{atomic, E} -> {atomic, E};
		Other -> 
			?log_error("tran_put error Rec=~p,Other=~p",[Rec,Other]),Other
	end.
dirty_put(Rec) ->
	case mnesia:dirty_write(Rec) of 
		ok -> ok;
		Other -> ?log_error("dirty_put error Rec=~p,Other=~p",[Rec,Other]),fail
	end.

tran_match(Tab,Pat)->
	case mnesia:transaction(fun() -> mnesia:match_object(Tab, Pat, read) end) of
        {atomic, E} -> E;
        Other -> ?log_error("tran_match error Tab=~p,Pat=~p,Other=~p",[Tab,Pat,Other]),[]
	end.

dirty_match(Tab,Pat)->
	case mnesia:dirty_match_object(Tab,Pat) of
		L when erlang:is_list(L) -> L;
		Other -> ?log_error("dirty_match error Tab=~p,Pat=~p,Other=~p",[Tab,Pat,Other]),[]
	end.

tran_del(Tab, Key) ->
  	case mnesia:transaction(fun() -> mnesia:delete({Tab, Key}) end) of
		{atomic, E} -> E;
		Other -> ?log_error("tran_del error Tab=~p,Key=~p,Other=~p",[Tab,Key,Other]),[]
	end.

dirty_del(Tab, Key) ->
  	mnesia:dirty_delete(Tab, Key).

dirty_select(Tab,MatchSpec)->
	case mnesia:dirty_select(Tab, MatchSpec) of
		L when erlang:is_list(L) -> L;
		Other -> ?log_error("dirty_select error Tab=~p,MatchSpec=~p,Other=~p",[Tab,MatchSpec,Other]),[]
	end.

clear(Tab) ->
	case mnesia:clear_table(Tab) of
		{atomic, ok} -> ok;
		Other ->
			?log_error("clear error Tab=~p,Other=~p",[Tab,Other]),error
	end.

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
getOrKeyLoadData(Tab,Key)->
	case db:dirty_get(Tab,Key) of  
		[]->dbm_worker:work({load, Tab, id, util:to_binary(Key)});
		Datas when erlang:is_list(Datas)->Datas;
		_->[]
	end.

getOrLoadData(Tab,Key,KeyPos,KeyName)->
	case db:dirty_get(Tab,Key,KeyPos) of  
		[]->dbm_worker:work({load, Tab, KeyName, util:to_binary(Key)});
		Datas when erlang:is_list(Datas)->Datas;
		_->[]
	end.
getOrFindData(Tab,Key,KeyPos,KeyName)->
	case db:dirty_get(Tab,Key,KeyPos) of  
		[]->dbm_worker:work({find, Tab, KeyName, util:to_binary(Key)});
		Datas when erlang:is_list(Datas)->Datas;
		_->[]
	end.
getOrKeyFindData(Tab,Key)->
	case db:dirty_get(Tab,Key) of 
		[]->dbm_worker:work({find, Tab, id, util:to_binary(Key)});
		Datas when erlang:is_list(Datas)->Datas;
		_->[]
	end.


init_ets_table(Tab,KeyIndex) ->
	ets:new(Tab, [set,public,named_table,{keypos, KeyIndex}]).
get_ets_data(Tab,Key) ->
	case ets:lookup(Tab, Key) of
       Datas when erlang:is_list(Datas)->Datas;
       Other -> ?log("Error:get_ets_data error,Tab =~p,key=~p,Other=~p",[Tab,Key,Other]),?UNDEFINED
    end.
match_ets_data(Tab,Pattern) ->
	case ets:match_object(Tab, Pattern) of 
		 Datas when erlang:is_list(Datas)-> Datas;
		 Other -> ?log("Error:match_ets_data error,Tab =~p,Pattern=~p,Other=~p",[Tab,Pattern,Other]),?UNDEFINED
	end.
%% 	 MatchHead = #ply{lev='$1', _ = '_'},
%% 	 Guards = [{'=<', '$1',?PUSH_CONDITION}],
%% 	 case db:dirty_select(ply, [{MatchHead, Guards, ['$_']}]) of
select_ets_data(Tab,MatchSpec) ->
	case ets:select(Tab, MatchSpec)of 
		 Datas when erlang:is_list(Datas)->Datas;
		 Other -> ?log("Error:match_ets_data error,Tab =~p,MatchSpec=~p,Other=~p",[Tab,MatchSpec,Other]),?UNDEFINED
	end.
put_ets_data(Tab,Data) ->  
	ets:insert(Tab,Data).
del_ets_data(Tab,Key) ->
	ets:delete(Tab, Key).






	




€
put_obj_datas(Class,Key,Val)->
	put({Class,Key},Val).
get_obj_datas(Class,Key)->
	get({Class,Key}).
del_obj_datas(Class,Key)->
	erase({Class,Key}).
get_class_objs(Class)->
	Keys=erlang:get_keys(),
	Fun=fun(ObjKey,Res)->  
				case  ObjKey of 
					{Class,_}->[get(ObjKey)|Res];
					_->Res
				end   end,
	lists:foldl(Fun, [], Keys).

filter_class_objs(Class,Pred)->
	Objs=get_class_objs(Class),
	lists:filter(Pred, Objs).

del_class_objs(Class)->
	Keys=erlang:get_keys(),
	Fun=fun(ObjKey)->  
				case  ObjKey of 
					{Class,K}->erase({Class,K});
					_->skip
				end   end,
	lists:foreach(Fun, Keys).


