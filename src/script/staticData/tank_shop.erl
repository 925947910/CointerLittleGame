%% @author Administrator
%% @doc @todo Add description to tank_shop.


-module(tank_shop).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================

-export([get_data/1,get_ids/0]).
get_data(1)->#tank_shop{id=1,coin=800,type=1,item_field="Skins",desc="buy_skin:1"};
get_data(2)->#tank_shop{id=2,coin=800,type=1,item_field="Skins",desc="buy_skin:2"};
get_data(3)->#tank_shop{id=3,coin=800,type=1,item_field="Skins",desc="buy_skin:3"};
get_data(4)->#tank_shop{id=4,coin=800,type=1,item_field="Skins",desc="buy_skin:4"};
get_data(5)->#tank_shop{id=5,coin=800,type=1,item_field="Skins",desc="buy_skin:5"};
get_data(6)->#tank_shop{id=6,coin=800,type=1,item_field="Skins",desc="buy_skin:6"};
get_data(7)->#tank_shop{id=7,coin=800,type=1,item_field="Skins",desc="buy_skin:7"};
get_data(8)->#tank_shop{id=8,coin=1800,type=1,item_field="Skins",desc="buy_skin:8"};
get_data(9)->#tank_shop{id=9,coin=1800,type=1,item_field="Skins",desc="buy_skin:9"};
get_data(10)->#tank_shop{id=10,coin=1800,type=1,item_field="Skins",desc="buy_skin:10"};
get_data(11)->#tank_shop{id=11,coin=1800,type=1,item_field="Skins",desc="buy_skin:11"};
get_data(12)->#tank_shop{id=12,coin=1800,type=1,item_field="Skins",desc="buy_skin:12"};
get_data(13)->#tank_shop{id=13,coin=1800,type=1,item_field="Skins",desc="buy_skin:13"};
get_data(14)->#tank_shop{id=14,coin=1800,type=1,item_field="Skins",desc="buy_skin:14"};
get_data(15)->#tank_shop{id=15,coin=1800,type=1,item_field="Skins",desc="buy_skin:15"};
get_data(16)->#tank_shop{id=16,coin=1800,type=1,item_field="Skins",desc="buy_skin:16"};
get_data(17)->#tank_shop{id=17,coin=1800,type=1,item_field="Skins",desc="buy_skin:17"};
get_data(18)->#tank_shop{id=18,coin=1800,type=1,item_field="Skins",desc="buy_skin:18"};
get_data(19)->#tank_shop{id=19,coin=1800,type=1,item_field="Skins",desc="buy_skin:19"};
get_data(20)->#tank_shop{id=20,coin=2800,type=1,item_field="Skins",desc="buy_skin:20"};
get_data(_)->?UNDEFINED.

get_ids()->[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20].


%% ====================================================================
%% Internal functions
%% ====================================================================


