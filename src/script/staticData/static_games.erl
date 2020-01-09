%% @author Administrator
%% @doc @todo Add description to static_games.


-module(static_games).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-export([get_data/1,get_ids/0]).
get_data(1)->#static_game{id=1,name="贪吃蛇蛇",mod=run_snake,matchTime=30,shop=snake_shop,item_key="snake_item:"};
get_data(2)->#static_game{id=2,name="斗币坦克大战",mod=run_tank,matchTime=15,shop=tank_shop,item_key="tank_item:"};
get_data(3)->#static_game{id=3,name="斗币大咖跑",mod=run_bossRun,matchTime=15,shop=bossRun_shop,item_key="bossRun_item:"};
get_data(4)->#static_game{id=4,name="斗币炮炮弹",mod=run_paopaoDan,matchTime=15,shop=paopaoDan_shop,item_key="paopaoDan_item:"};
get_data(_)->?UNDEFINED.

get_ids()->[1,2,3,4].


%% ====================================================================
%% Internal functions
%% ====================================================================


