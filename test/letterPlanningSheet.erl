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
%%            Carl Wright (wright@servicelevel.net)
%% Last Edit: 2010-04-14
%% =====================================================================

-module(letterPlanningSheet).
-import(pdf_op, [n2s/1]).
-import(eg_pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).


%%
%%  This produces a planning sheet for US letter size paper.
%%
test()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,letter),
    eg_pdf:set_page(PDF,1),
    showGrid(PDF, letter),
    eg_pdf:set_font(PDF,"Times-Roman", 36),
    Base = 575,
    moveAndShow(PDF, 50,Base, "Letter template planning sheet-"),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("../test/letter_planning_sheet.pdf",[Serialised]),
    eg_pdf:delete(PDF).














