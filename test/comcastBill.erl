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
-import(eg_pdf_op, [n2s/1]).
-import(eg_pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).


%%
%%  This produces an example Comcast bill.
%%
test()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,letter),
    eg_pdf:set_page(PDF,1),
   %% pdf_lib:showGrid(PDF, letter),

    eg_pdf:set_dash(PDF, [1]),
    eg_pdf:set_stroke_gray(PDF, 0.5), %% black
    eg_pdf:set_line_width(PDF,1),
    eg_pdf:round_rect(PDF, {360,705},{210,75}, 5),
    eg_pdf:path(PDF, stroke),
    eg_pdf:set_font(PDF,"Helvetica", 10),
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
    
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF,dodgerblue),
    eg_pdf:set_line_width(PDF,2),
    eg_pdf:line(PDF, 25,700,570,700), 
    
    eg_pdf:image(PDF, "../test/comcast_logo.jpg",{25,760},{height,20}),

    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,orange),
    eg_pdf:round_top_rect(PDF, {320,675},{250,20}, 10),
    eg_pdf:path(PDF, fill),
    eg_pdf:restore_state(PDF),

    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,moccasin),
    eg_pdf:rectangle(PDF, 320,620,250,55, fill),
    eg_pdf:restore_state(PDF),
    
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,gainsboro),
    eg_pdf:rectangle(PDF, 320,590,250,30, fill),
    eg_pdf:restore_state(PDF),
    
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,orange),
    eg_pdf:round_top_rect(PDF, {320,560},{250,20}, 10),
    eg_pdf:path(PDF, fill),
    eg_pdf:restore_state(PDF),

    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,moccasin),
    eg_pdf:rectangle(PDF, 320,540,250,20, fill),
    eg_pdf:restore_state(PDF),
    
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,gainsboro),
    eg_pdf:rectangle(PDF, 320,520,250,20, fill),
    eg_pdf:restore_state(PDF),
     
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,yellowgreen),
    eg_pdf:round_rect(PDF, {320,470}, {250,40}, 20),
    eg_pdf:path(PDF, fill),
    eg_pdf:restore_state(PDF),  
    
    eg_pdf:image(PDF, "../test/HighSpeedInternet.jpg",{322,543},{height,15}), 
    
    eg_pdf:set_fill_gray(PDF, 1.0),
    eg_pdf:set_font(PDF,"Helvetica", 14),
    moveAndShow(PDF, 325,680,                    "Monthly Statement Summary"),
    moveAndShow(PDF, 325,565,                    "New Charges Summary"),
    eg_pdf:set_fill_gray(PDF, 0.0),
    moveAndShow(PDF, 25,675,                      "SERVICE LEVEL CORP"),

    eg_pdf:set_font(PDF,"Helvetica", 12),
    moveAndShow(PDF, 25,650,"For service at:"),
    moveAndShow(PDF, 25,635,"7006 SUNCREST DR"),
    moveAndShow(PDF, 25,620,"SALINE MI 48176-9102"),
    
    eg_pdf:set_font(PDF,"Helvetica", 14),
    moveAndShow(PDF, 25,600,"News from Comcast"),  

    eg_test3:zap(PDF, xml(news), 25, 580, 40, 10, 12, 20),
           
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    ok = file:write_file("../test/comcast_bill.pdf",[Serialised]),
    eg_pdf:delete(PDF).



xml(news) ->
  "<p>IMPORTANT INFORMATION - Our network enhancement is complete! </p>";
  
xml(2) -> "<p> On 4/21/2010, any TV, including QAN tuner TVx, w\/o a Comcast
Digital device will only be able to receive Limited Basic channels 2-24, 95, 
96 and 99. Just visit us online at comcast.com/digitnow or call us at 1-877-634-4434
to get your equipment today.</p>";
xml(3) -> "<p> All of us at Comcast extend our apreciation and thanks for being our 
customer, your opinion counts!!</p> <p>Please let us know any way we may better 
serve your needs.</p>".







