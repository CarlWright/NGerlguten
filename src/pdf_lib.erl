%%======================================================================
%% Purpose: PDF library routines
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
%% Last Edit: 2003-03-19
%% =====================================================================

-module(pdf_lib).

-export([draw_box/6, showGrid/2, moveAndShow/4, moveAndShow/5,
	 moveAndShowRot/5, code128/1]).
-import(pdf_op, [i2s/1, n2s/1]).

%% showGrid(PDF, a4 | usLetter) 
%%   adds a grid to the current page page

sizeOfPaper(a4) ->
    {595, 842};
sizeOfPaper(usLetter) ->
    {612, 792}.

showGrid(PDF, Paper) ->
    {PaperWidth, PaperHeight} = sizeOfPaper(Paper),
    %% Top = PaperHeight - 10,
    Top = 825, % hack
    Bottom = 10,
    Left = 10,
    %% Right = PaperWidth - 10,
    Right = 575,
    pdf:set_font(PDF,"Helvetica", 8),
    vlines(PDF, Left, Right, Top, Bottom),
    hlines(PDF, Left, Right, Top, Bottom).

hlines(PDF, Left, Right, Top, Bottom) ->
    diter(Top,25,10,
	  fun(Y) ->
		  %% pdf:set_fill_gray(PDF,1.0),
		  pdf:line(PDF, Left, Y, Left+20, Y),
		  pdf:line(PDF, Right, Y, Right-20, Y),
		  %% pdf:set_fill_gray(PDF,0.8),
		  pdf:line(PDF, Left+20,Y,Right-20,Y),
		  moveAndShow(PDF, Left, Y+2, n2s(Y)),
		  moveAndShow(PDF, Right-20, Y+2, n2s(Y)),
		  true
	  end).

vlines(PDF, Left, Right, Top, Bottom) ->
    diter(Right,25,10,
	  fun(X) ->
		  pdf:line(PDF, X, Top, X, Top-20),
		  moveAndShow(PDF, X-5, Top-35,n2s(X)),
		  pdf:line(PDF, X, Bottom, X, Bottom+20),
		  pdf:line(PDF, X, Top -40, X, Bottom + 35),
		  moveAndShow(PDF, X-5, Bottom+23,n2s(X))
	  end).

moveAndShow(PDF, X, Y, Str) ->
    pdf:begin_text(PDF),
    pdf:set_text_pos(PDF, X, Y),
    pdf:text(PDF, Str),
    pdf:end_text(PDF).

moveAndShowRot(PDF, X, Y, Str, Rot) ->
    pdf:save_state(PDF),
    pdf:begin_text(PDF),
    pdf:rotate(PDF, Rot),
    pdf:set_text_pos(PDF, X, Y),
    pdf:text(PDF, Str),
    pdf:end_text(PDF),
    pdf:restore_state(PDF).

moveAndShow(PDF, X, Y, Str, Scale) ->
    pdf:begin_text(PDF),
    pdf:set_text_pos(PDF, X, Y),
    pdf:set_text_scale(PDF, Scale),
    pdf:text(PDF, Str),
    pdf:set_text_scale(PDF, 100),
    pdf:end_text(PDF).

%% downwards iterator

diter(X, Inc, Stop, F) when X < Stop ->
    true;
diter(X, Inc, Stop, F) ->
    F(X), diter(X-Inc,Inc,Stop,F).

draw_box(PDF, X, Y, Measure, Lines, MaxRows) ->    
    pdf:append_stream(PDF, draw_box1(X, Y, Measure, Lines, MaxRows)).

draw_box1(X1,Y1,Measure,Leading,MaxRows) ->
    %% X1,Y1,Leading are in points
    %% Measure        is in picas
    X2 = X1 + Measure,
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

-define(B_SHIFT_C, 99).
-define(C_SHIFT_B, 100).
-define(START_A, 103).
-define(START_B, 104).
-define(START_C, 105).
-define(STOP,    106).

-define(DIGIT(X), X >= $0, X =< $9). 
-define(l2i(X), list_to_integer(X)).

code128(String0) ->
    {Start,Mode} = code128_start(String0),
    String1 = code128_conv(Mode, String0, []),
    CheckChar = code128_chk(String1, Start),
    Result = lists:flatten([Start|String1]++[CheckChar,?STOP]),
    [code128_trans(X) || X <- Result].

code128_trans(X) when X >= 95 -> 
    X + 97;
code128_trans(X) ->
    X + 32.

code128_chk(String, StartChar) ->
    F = fun(C, {N,Acc}) -> {N+1, Acc+(N*C)} end,
    {_,Sum} = lists:foldl(F, {1, StartChar}, String),
    (Sum rem 103).
				       
code128_conv(_, [], Acc) ->
    lists:reverse(Acc);

code128_conv(c, [A,B,C,D|R], Acc) when ?DIGIT(A),?DIGIT(B),?DIGIT(C),?DIGIT(D) ->
    code128_conv(c, R, [?l2i([C,D]),?l2i([A,B])|Acc]);
code128_conv(c, R, Acc) ->
    code128_conv(b, R, [?C_SHIFT_B|Acc]);

code128_conv(b, R=[A,B,C,D|_], Acc) when ?DIGIT(A),?DIGIT(B),?DIGIT(C),?DIGIT(D) ->
    code128_conv(c, R, [?B_SHIFT_C|Acc]);
code128_conv(b, [C|R], Acc) ->
    code128_conv(b, R, [C-32|Acc]).

code128_start([A,B,C,D|_]) when ?DIGIT(A),?DIGIT(B),?DIGIT(C),?DIGIT(D) ->
    {?START_C,c};
code128_start(String0) ->
    {?START_B,b}.

