%% @author Administrator
%% @doc @todo Add description to snake_shop.


-module(snake_shop).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================

-export([get_data/1,get_ids/0]).
get_data(1)->#snake_shop{id=1,coin=2800,type=1,item_field="Skins",desc="buy_skin:1"};
get_data(2)->#snake_shop{id=2,coin=2800,type=1,item_field="Skins",desc="buy_skin:2"};
get_data(3)->#snake_shop{id=3,coin=2800,type=1,item_field="Skins",desc="buy_skin:3"};
get_data(4)->#snake_shop{id=4,coin=2800,type=1,item_field="Skins",desc="buy_skin:4"};
get_data(5)->#snake_shop{id=5,coin=1800,type=1,item_field="Skins",desc="buy_skin:5"};
get_data(6)->#snake_shop{id=6,coin=1800,type=1,item_field="Skins",desc="buy_skin:6"};
get_data(7)->#snake_shop{id=7,coin=1800,type=1,item_field="Skins",desc="buy_skin:7"};
get_data(8)->#snake_shop{id=8,coin=1800,type=1,item_field="Skins",desc="buy_skin:8"};
get_data(_)->?UNDEFINED.

get_ids()->[1,2,3,4,5,6,7,8].


%% ====================================================================
%% Internal functions
%% ====================================================================


