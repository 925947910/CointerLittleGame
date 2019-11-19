%% @author Administrator
%% @doc @todo Add description to snake_data.


-module(bossRun_game).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_data/1,get_ids/0]).
get_data(1)->#bossRun_game{id=1,coin=0,maxPlayer=3,minPlayer=1,totalScore=80};

get_data(_)->?UNDEFINED.

get_ids()->[1].
%% ====================================================================
%% Internal functions
%% ====================================================================


