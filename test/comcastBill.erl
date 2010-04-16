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
%% Authors:   Carl Wright (wright@servicelevel.net)
%%            
%% Last Edit: 2010-04-14
%% =====================================================================

-module(comcastBill).
-import(pdf_op, [n2s/1]).
-import(pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).


%%
%%  This produces an example Comcast bill.
%%
test()->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,letter),
    pdf:set_page(PDF,1),
   %% pdf_lib:showGrid(PDF, letter),

    pdf:set_dash(PDF, [1]),
    pdf:set_stroke_gray(PDF, 0.5), %% black
    pdf:set_line_width(PDF,1),
    pdf:round_rect(PDF, {360,705},{210,75}, 5),
    pdf:path(PDF, stroke),
    pdf:set_font(PDF,"Helvetica", 10),
    Base = 765,
    Increment = 12,
    moveAndShow(PDF, 370,Base,                    "Account Number"),
    moveAndShow(PDF, 370,Base - Increment,        "Billing Date"),
    moveAndShow(PDF, 370,Base - (2 * Increment),  "Total Amount Due"),
    moveAndShow(PDF, 370,Base - (3 * Increment),  "Payment Due by"),
    moveAndShow(PDF, 470,Base,                    "09588 355469-01-5"),
    moveAndShow(PDF, 470,Base - Increment,        "02/28/10"),
    moveAndShow(PDF, 470,Base - (2 * Increment),  "$99.95"),
    moveAndShow(PDF, 470,Base - (3 * Increment),  "Page 1 of 2"),  
    
    pdf:set_dash(PDF, solid),
    pdf:set_stroke_color(PDF,dodgerblue),
    pdf:set_line_width(PDF,2),
    pdf:line(PDF, 25,700,570,700), 
    
    
    Serialised = pdf:export(PDF),
    file:write_file("../test/comcast_bill.pdf",[Serialised]),
    pdf:delete(PDF).














