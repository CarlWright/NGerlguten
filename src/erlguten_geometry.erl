%%======================================================================
%% erlguten_geometry.erl - 
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Joe Armstrong
%%
%%   General Terms
%%
%%   Erlguten  is   free  software.   It   can  be  used,   modified  and
%% redistributed  by anybody for  personal or  commercial use.   The only
%% restriction  is  altering the  copyright  notice  associated with  the
%% material. Individuals or corporations are permitted to use, include or
%% modify the Erlguten engine.   All material developed with the Erlguten
%% language belongs to their respective copyright holder.
%% 
%%   Copyright Notice
%% 
%%   This  program is  free  software.  It  can  be redistributed  and/or
%% modified,  provided that this  copyright notice  is kept  intact. This
%% program is distributed in the hope that it will be useful, but without
%% any warranty; without even  the implied warranty of merchantability or
%% fitness for  a particular  purpose.  In no  event shall  the copyright
%% holder  be liable  for  any direct,  indirect,  incidental or  special
%% damages arising in any way out of the use of this software.
%%
%% Authors:   Joe Armstrong <joe@sics.se>
%% Last Edit: 2003-03-11
%% =====================================================================


-module(erlguten_geometry).

-export([draw_box/5,
	 mk_line_headers/6, paraShape_measure/1, paraShape_next/1]).

-import(lists, [reverse/1, reverse/2]).
-import(pdf,  [i2s/1]).

%%--------------------------------------------------------------------
%% Code generation examples:

%%  1 0 0 1 20 750 Tm
%%  18    0 Td (Line 1 indent 18 point) Tj
%%  -18 -16 Td (line 2 indebt 0) Tj
%%  0   -16 Td (line 3 indent 0) Tj
%%  0   -16 Td (line 4 indent 0) Tj
%%  12  -16 Td (line 5 indent 12 points) Tj
%%  -12 -16 Td (line 6 indent 0 ) Tj

%% Line = [{Offset, Measure}] -> [Cmds] 
%%   Cmds = set of commands to preceed every line

%% ParaShape = [Len,Len,...]

mk_line_headers(X, Y, Measure, Leading, ParaShape, N) when N > 0 ->
    %% io:format("Measure=~p ParaShape=~p~n",[Measure, ParaShape]),
    Offset = Measure - paraShape_measure(ParaShape),
    Cmd = ["1 0 0 1 ",i2s(X)," ",i2s(Y)," Tm "],
    C1 = case Offset of
	     0 -> [];
	     _ -> [i2s(Offset*12)," 0 Td "]
	 end,
    mk_line_headers1(Offset, Measure, Leading, paraShape_next(ParaShape), 
		     N-1, [[Cmd,C1]]).

mk_line_headers1(Offset, Measure, Leading, P, 0, L) ->
    reverse(L);
mk_line_headers1(Offset1, Measure, Leading, P, N, L) ->
    Offset2  = Measure - paraShape_measure(P),
    P1 = paraShape_next(P),
    Cmd = if
	      Offset1 == Offset2 ->
		  ["0 -",i2s(Leading)," Td "];
	      true ->
		  Dx = Offset2 - Offset1,
		  [i2s(Dx*12)," -",i2s(Leading)," Td "]
	  end,
    mk_line_headers1(Offset2, Measure, Leading, P1, N-1, [Cmd|L]).

    
paraShape_measure([H|_]) -> H.

paraShape_next([H])   -> [H];
paraShape_next([H|T]) -> T.

draw_box(X1,Y1,Measure,Leading,MaxRows) ->
    %% X1,Y1,Leading are in points
    %% Measure        is in picas
    X2 = X1 + Measure*12,
    Y2 = Y1 - Leading*MaxRows, 
    [" q  0.4 g 0.4 G 0 w ",
     %% verticals
     line(X1,Y1,X1,Y2),
     line(X2,Y1,X2,Y2),
     for(0, MaxRows,
	 fun(I) ->
		 Y = Y1 - I*Leading,
		 line(X1,Y,X2,Y)
	 end),
     " Q "].

for(I, Max, F) when I > Max -> [];
for(I, Max, F)              -> [F(I)|for(I+1,Max,F)].

line(X1,Y1,X2,Y2) -> [i2s(X1)," ",i2s(Y1)," m ",i2s(X2)," ",i2s(Y2)," l S "].

