%% @author Administrator
%% @doc @todo Add description to snake_data.


-module(paopaoDan_game).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_data/1,get_ids/0]).
get_data(1)->#paopaoDan_game{id=1,coin=0,maxPlayer=2,minPlayer=2,totalScore=80};

get_data(_)->?UNDEFINED.

get_ids()->[1].
%% ====================================================================
%% Internal functions
%% ====================================================================


