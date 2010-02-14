%%======================================================================
%% Purpose: Grid planning sheet
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
%% Last Edit: 2003-03-12
%% =====================================================================

-module(erlguten_test4).
-import(pdf, [n2s/1]).
-export([test/0]).

test()->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,a4),
    pdf:set_page(PDF,1),
    showGrid(PDF, a4),
    pdf:set_font(PDF,"Times-Roman", 36),
    moveAndShow(PDF, 50,700, "A4 template planning sheet-"),
    moveAndShow(PDF, 50,650, "Use this sheet to see the"),
    moveAndShow(PDF, 50,600, "co-ordinates of your objects."),
    Serialised = pdf:export(PDF),
    file:write_file("erlguten_test4.pdf",[Serialised]),
    pdf:delete(PDF).

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

%% downwards iterator

diter(X, Inc, Stop, F) when X < Stop ->
    true;
diter(X, Inc, Stop, F) ->
    F(X), diter(X-Inc,Inc,Stop,F).
    






