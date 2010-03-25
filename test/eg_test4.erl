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

-module(eg_test4).
-import(pdf_op, [n2s/1]).
-import(pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).

test()->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,a4),
    pdf:set_page(PDF,1),
    pdf_lib:showGrid(PDF, a4),
    pdf:set_font(PDF,"Times-Roman", 36),
    Base = 575,
    moveAndShow(PDF, 50,Base, "A4 template planning sheet-"),
    moveAndShow(PDF, 50,Base-50, "Use this sheet to see the"),
    moveAndShow(PDF, 50,Base-100, "co-ordinates of your objects."),
    pdf:set_font(PDF, "GoodCityModern", 60),
    moveAndShow(PDF, 50, 675, "ErlGuten-3.0 coming soon"),
    pdf:set_font(PDF,"Times-Italic", 16),
    moveAndShow(PDF, 50, 625, "With font embedding - the above is similar to"
		" Gutenbergs original font"),
    moveAndShow(PDF, 50, 610, "used in the 42 line bible - see page 2 for "
		"more examples"),
    pdf:new_page(PDF),
    pdf:set_page(PDF,2),
    Fonts = lists:sort(egFontMap:allFonts()),
    showem(PDF, 820, Fonts),
    Serialised = pdf:export(PDF),
    file:write_file("../test/eg_test4.pdf",[Serialised]),
    pdf:delete(PDF).

showem(PDF, Y, [H|T]) ->
    pdf:set_font(PDF, "Times-Roman", 20),
    moveAndShow(PDF, 20,Y, H),
    pdf:set_font(PDF,H, 20),
    moveAndShow(PDF, 225, Y, "abcdefg ABCDEFG 123456890"),
    showem(PDF, Y-22, T);
showem(PDF, _, []) ->
    true.














