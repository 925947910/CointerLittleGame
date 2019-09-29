%% @author Administrator
%% @doc @todo Add description to static_games.


-module(static_games).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([get_data/1,get_ids/0]).
get_data(1)->#static_game{id=1,name="斗币蛇",mod=run_snake,shop=snake_shop,item_key="snake_item:"};
get_data(2)->#static_game{id=1,name="斗币坦克大战",mod=run_tank,shop=tank_shop,item_key="tank_item:"};
get_data(_)->?UNDEFINED.

get_ids()->[1,2].


%% ====================================================================
%% Internal functions
%% ====================================================================


