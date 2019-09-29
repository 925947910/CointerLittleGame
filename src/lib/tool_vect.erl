-module(tool_vect).
-include("common.hrl").

-export([radian2angle/1,angle2radian/1,is_null/1]).
-export([add/2,dec/2,ride/2,divide/2,format/1,near/1,floor/1]).
-export([s2d_length/1,s3d_length/1]).
-export([s2d_dot_ride/2,s3d_dot_ride/2]).
-export([s2d_get_radian/2,s3d_get_radian/2]).
-export([s2d_rotate_radian/2,s3d_rotate_radian/2]).
-export([s2d_get_vect_by_dir/1,s3d_get_vect_by_dir/1]).
-export([s2d_dot_line_dis/2,s2d_get_line_point/2,s2d_get_dir/1,point_in_recf/4]).
-define(PI,3.1415926535897932).
-define(PI2, (?PI * 2)).

radian2angle(R) when R >= ?PI2 -> radian2angle(R - util:floor(R / ?PI2) * ?PI2);
radian2angle(R) when R < 0 -> radian2angle(R + util:ceil( (-1 * R) / ?PI2) * ?PI2);
radian2angle(R) -> R / ?PI2 * 360.

angle2radian(R) when R >= 360 -> angle2radian(R - util:floor(R / 360) * 360);
angle2radian(R) when R < 0 -> angle2radian(R + util:ceil( (-1 * R) / 360) * 360);
angle2radian(R) -> R / 360 * ?PI2.

is_null({V1,V2}) when V1 == 0 andalso V2 == 0 -> true;
is_null({V1,V2,V3})when V1 == 0 andalso V2 == 0  andalso V3 == 0  -> true;
is_null(_) -> false.

add({X1,Y1},{X2,Y2}) -> {X1 + X2 , Y1 + Y2};
add({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1 + X2 , Y1 + Y2 , Z1 + Z2}.

dec({X1,Y1},{X2,Y2}) -> {X1 - X2 , Y1 - Y2};
dec({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1 - X2 , Y1 - Y2 , Z1 - Z2}.

ride({X,Y},D) -> {X * D , Y * D};
ride({X,Y,Z},D) -> {X * D, Y * D , Z * D}.

divide({X,Y},D) -> {X / D , Y / D};
divide({X,Y,Z},D) -> {X / D, Y / D , Z / D}.

format(V = {X,Y}) -> {X / s2d_length(V),Y / s2d_length(V)};
format(V = {X,Y,Z}) -> {X / s3d_length(V),Y / s3d_length(V),Z / s3d_length(V)}.

near({X,Y}) -> {util:floor(X + 0.5),util:floor(Y + 0.5)};
near({X,Y,Z}) -> {util:floor(X + 0.5),util:floor(Y + 0.5),util:floor(Z + 0.5)}.

floor({X,Y}) -> {util:floor(X),util:floor(Y)};
floor({X,Y,Z}) -> {util:floor(X),util:floor(Y),util:floor(Z)}.

s2d_length({X,Y}) -> math:sqrt(X * X + Y * Y).
s3d_length({X,Y,Z}) -> math:sqrt(X * X + Y * Y + Z * Z).

s2d_dot_ride({X1,Y1},{X2,Y2}) -> X1 *X2 + Y1 * Y2.
s3d_dot_ride({X1,Y1,Z1},{X2,Y2,Z2}) -> X1 *X2 + Y1 * Y2 + Z1 * Z2.

s3d_get_radian(V1,V2) ->
	Null1 = is_null(V1),
	Null2 = is_null(V2),
	if
		Null1 == false andalso Null2 == false -> 
			R = s3d_dot_ride(V1,V2) / (s3d_length(V1) * s3d_length(V2)),
			RR = if
					 R > 1 -> 1;
					 R < -1 -> -1;
					 true -> R
				 end,
			math:acos(RR);
		true -> 0
	end.
s2d_get_radian(V1,V2) ->
	Null1 = is_null(V1),
	Null2 = is_null(V2),
	if
		Null1 == false andalso Null2 == false -> 
			R = s2d_dot_ride(V1,V2) / (s2d_length(V1) * s2d_length(V2)),
			RR = if
					 R > 1 -> 1;
					 R < -1 -> -1;
					 true -> R
				 end,
			math:acos(RR);
		true -> 0
	end.

s3d_rotate_radian({X,Y,Z},{RX,RY,RZ})-> 
	%%x
	{Y1,Z1} = s2d_rotate_radian({Y,Z},RX),
	%%y
	{X1,Z2} = s2d_rotate_radian({X,Z1},RY),
	%%z
	{X2,Y2} = s2d_rotate_radian({X1,Y1},RZ),
	{X2,Y2,Z2}.
s2d_rotate_radian({X,Y},R) ->  {X * math:cos(R) + Y * math:sin(R),Y * math:cos(R) - X * math:sin(R)}.

s2d_get_vect_by_dir(R) ->
	s2d_rotate_radian({0,1},R).
s3d_get_vect_by_dir(R) ->
	s3d_rotate_radian({0,0,1},R).

s2d_dot_line_dis({X,_},{PX,_}) when X == 0 -> util:abs(PX);
s2d_dot_line_dis({_,Y},{_,PY}) when Y == 0 -> util:abs(PY);
s2d_dot_line_dis({LX,LY},{PX,PY}) -> 
	try
		util:abs((LY / LX) * PX - PY) / s2d_length({(LY / LX),1})
	catch _E:_R -> ?log_error("s2d_dot_line_dis l=~p,p=~p",[{LX,LY},{PX,PY}]),0
	end.



line_point_x(X,SY,EY,List) when SY > EY -> line_point_x(X,SY - 1,EY,lists:append(List,[{X,SY}]));
line_point_x(X,SY,EY,List) when SY == EY -> lists:append(List,[{X,SY}]);
line_point_x(X,SY,EY,List) -> line_point_x(X,SY + 1,EY,lists:append(List,[{X,SY}])).
line_point_y(Y,SX,EX,List) when SX > EX -> line_point_y(Y,SX - 1,EX,lists:append(List,[{SX,Y}]));
line_point_y(Y,SX,EX,List) when SX == EX -> lists:append(List,[{SX,Y}]);
line_point_y(Y,SX,EX,List) -> line_point_y(Y,SX + 1,EX,lists:append(List,[{SX,Y}])).

s2d_get_line_point({SX,SY},{EX,EY}) ->
	Lenx = util:abs(SX - EX),
	Leny = util:abs(SY - EY),	
	if
		Lenx == 0 -> line_point_x(SX,SY,EY,[]);
		Leny == 0 -> line_point_y(SY,SX,EX,[]);
		true -> 
			R = Leny  / Lenx,
			{ok,List} = if
				R < 1 -> 
					Peri = (EX - SX) / Lenx,
					Fun = fun(I,List) -> {ok,lists:append(List,[{util:floor(SX + Peri * I),SY + util:floor((EY - SY)  / (EX - SX) * (Peri * I) + 0.5)}])} end,
					util:for(0, Lenx,Fun, []);
				true -> 
					Peri = (EY - SY) / Leny,
					Fun = fun(I,List) -> {ok,lists:append(List,[{SX + util:floor((EX - SX)  / (EY - SY) * (Peri * I) + 0.5),util:floor(SY + Peri * I)}])}  end,
					util:for(0, Leny,Fun,[])
			end,
			List
	end.

s2d_get_dir(V = {X,_}) ->
	A1 = radian2angle(s2d_get_radian({0,1},V)),
	if
		X < 0 -> 360 - util:floor(A1 + 0.5);
		true -> util:floor(A1 + 0.5)
	end.


			
point_in_recf({TX,TY},{PX,PY},H,W)->
	
   {CX,CY}={PX,PY+H/2},
   
   
   C1=dis(CX-TX)=<W,
    
   C2=dis(CY-TY)=<H/2,
    
   	  C1 andalso C2.
dis(X)->
	if  
		X<0->0-X;
		true->X  
	end.	





	
	
  
  
  
   
   
    
    get_turn_point(Point,Dir,TurnAngle,Dis)->
	add(Point,ride(format(s2d_rotate_radian(s2d_get_vect_by_dir(angle2radian(Dir)),angle2radian(TurnAngle))),Dis)).

	
	