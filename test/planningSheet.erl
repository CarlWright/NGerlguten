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

-module(planningSheet).
-import(eg_pdf_op, [n2s/1]).
-import(eg_pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).


%%
%%  This produces a planning sheet for a selected size paper.
%%
test()->
    SheetSize = io:get_line("Sheet Size (see eg_pdf:pagesize) : "),
    SheetName = string:to_lower(string:strip(SheetSize -- "\n")),
    Size = list_to_atom(SheetName),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,Size),
    {0,0,Width,Height} = eg_pdf:pagesize(Size),
    eg_pdf:set_page(PDF,1),
    showGrid(PDF, Size),
    
    TestString = "Width = " ++ n2s(Width) ++ " -- Height = " ++ n2s(Height),
    LabelString = SheetName ++ " template planning sheet-",
    Stringsize = eg_pdf:get_string_width(PDF, "Times-Roman", 36, TestString),
    TargetSize = 24,
    Indent = round(Width * 0.15),
    FontSize = round(TargetSize * (((Width - (2 * Indent)) / Stringsize ))),
    eg_pdf:set_font(PDF,"Times-Roman", FontSize),
    Base = round(Height * 0.71),
    moveAndShow(PDF, Indent,Base, LabelString),

    moveAndShow(PDF, Indent, Base - (FontSize + 4), TestString),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    CustomName = "../test/" ++ SheetName ++ "_planning_sheet.pdf",
    io:format("Output to ~s~n",[CustomName]),
    ok = file:write_file(CustomName,[Serialised]),
    eg_pdf:delete(PDF).














