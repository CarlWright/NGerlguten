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

-module(eg_test6).
-import(pdf_op, [n2s/1]).
-import(pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).

test()->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,a4),
    pdf:set_page(PDF,1),
    %% pdf:set_font(PDF, "GoodCityModern", 40),
    pdf:set_font(PDF, "Victorias-Secret", 40),
    moveAndShow(PDF, 50, 700, "Hello Joe from Gutenburg"),

    Serialised = pdf:export(PDF),
    file:write_file("eg_test6.pdf",[Serialised]),
    pdf:delete(PDF).












