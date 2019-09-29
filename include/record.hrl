
-record(static_game,{id=1,name="",mod=snake,shop=0,item_key=""}).

-record(snake_game,{id=1,coin=1,maxPlayer=15,minPlayer=2,start=2000,over=300000,totalScore=0}).
-record(tank_game, {id=1,coin=1,maxPlayer=10,minPlayer=2,start=2000,over=300000,totalScore=0}).


-record(snake_shop,{id=1,coin=1,type=1,item_field="",desc=""}).
-record(tank_shop,{id=1,coin=1,type=1,item_field="",desc=""}).
