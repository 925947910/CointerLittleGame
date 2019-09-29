%% @author Administrator
%% @doc @todo Add description to snake_data.


-module(snake_game).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_data/1,get_ids/0]).
get_data(1)->#snake_game{id=1,coin=100,maxPlayer=15,minPlayer=2,totalScore=5000};
get_data(2)->#snake_game{id=2,coin=500,maxPlayer=15,minPlayer=2,totalScore=5000};
get_data(3)->#snake_game{id=3,coin=1000,maxPlayer=15,minPlayer=2,totalScore=5000};
get_data(4)->#snake_game{id=4,coin=10000,maxPlayer=15,minPlayer=2,totalScore=5000};
get_data(5)->?UNDEFINED.

get_ids()->[1,2,3,4].
%% ====================================================================
%% Internal functions
%% ====================================================================


