%% @author Administrator
%% @doc @todo Add description to snake_data.


-module(tank_game).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_data/1,get_ids/0]).
get_data(1)->#tank_game{id=1,coin=100,maxPlayer=2,minPlayer=2,totalScore=80};
get_data(2)->#tank_game{id=2,coin=100,maxPlayer=4,minPlayer=4,totalScore=80};
get_data(3)->#tank_game{id=3,coin=100,maxPlayer=6,minPlayer=6,totalScore=80};
get_data(4)->#tank_game{id=4,coin=100,maxPlayer=8,minPlayer=8,totalScore=80};

get_data(5)->#tank_game{id=5,coin=500,maxPlayer=2,minPlayer=2,totalScore=400};
get_data(6)->#tank_game{id=6,coin=500,maxPlayer=4,minPlayer=4,totalScore=400};
get_data(7)->#tank_game{id=8,coin=500,maxPlayer=6,minPlayer=6,totalScore=400};
get_data(8)->#tank_game{id=9,coin=500,maxPlayer=8,minPlayer=8,totalScore=400};

get_data(9)->#tank_game{id=9,coin=1000,maxPlayer=2,minPlayer=2,totalScore=800};
get_data(10)->#tank_game{id=10,coin=1000,maxPlayer=4,minPlayer=4,totalScore=800};
get_data(11)->#tank_game{id=11,coin=1000,maxPlayer=6,minPlayer=6,totalScore=800};
get_data(12)->#tank_game{id=12,coin=1000,maxPlayer=8,minPlayer=8,totalScore=800};

get_data(13)->#tank_game{id=13,coin=10000,maxPlayer=2,minPlayer=2,totalScore=8000};
get_data(14)->#tank_game{id=14,coin=10000,maxPlayer=4,minPlayer=4,totalScore=8000};
get_data(15)->#tank_game{id=15,coin=10000,maxPlayer=6,minPlayer=6,totalScore=8000};
get_data(16)->#tank_game{id=16,coin=10000,maxPlayer=8,minPlayer=8,totalScore=8000};
get_data(_)->?UNDEFINED.

get_ids()->[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16].
%% ====================================================================
%% Internal functions
%% ====================================================================


