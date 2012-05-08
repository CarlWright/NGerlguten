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

-module(comcast_bill_test).
-import(eg_pdf_op, [n2s/1]).
-import(eg_pdf_lib, [showGrid/2, moveAndShow/4, moveAndShowRot/5]).



%%
%%  This produces an example Comcast bill.
%%
run_test_()->
  Start = now(),
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
    Base = 760,
    Increment = 12,
    moveAndShow(PDF, 370,Base,                    "Account Number"),
    moveAndShow(PDF, 370,Base - Increment,        "Billing Date"),
    moveAndShow(PDF, 370,Base - (2 * Increment),  "Total Amount Due"),
    moveAndShow(PDF, 370,Base - (3 * Increment),  "Payment Due by"),
    moveAndShow(PDF, 470,Base,                    "09588 355496-01-5"),
    moveAndShow(PDF, 470,Base - Increment,        "02/28/10"),
    moveAndShow(PDF, 470,Base - (2 * Increment),  "$99.95"),
    moveAndShow(PDF, 470,Base - (3 * Increment),  "Page 1 of 2"),  
    
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF,dodgerblue),
    eg_pdf:set_line_width(PDF,2),
    eg_pdf:line(PDF, 25,700,570,700), 
    
    eg_pdf:image(PDF, "../testing/comcast_logo.jpg",{25,760},{height,20}),

    eg_pdf:set_fill_gray(PDF, 0.0),
    eg_pdf:set_font(PDF,"Helvetica", 10),
    moveAndShow(PDF, 30, 705,  "Contact us:"),  
    moveAndShow(PDF, 110,705,  "www.comcast.com"),  
    moveAndShow(PDF, 220,705,  "1-800-391-3000"),  
    
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
    
    eg_pdf:image(PDF, "../testing/HighSpeedInternet.jpg",{322,543},{height,15}), 
    
    eg_pdf:set_fill_gray(PDF, 1.0),
    eg_pdf:set_font(PDF,"Helvetica", 14),
    moveAndShow(PDF, 325,680,                    "Monthly Statement Summary"),
    moveAndShow(PDF, 325,565,                    "New Charges Summary"),
    eg_pdf:set_fill_gray(PDF, 0.0),
    eg_pdf:set_font(PDF,"Helvetica", 12),
    moveAndShow(PDF, 25,675,                      "SERVICE LEVEL CORP"),

    eg_pdf:set_font(PDF,"Helvetica", 10),
    moveAndShow(PDF, 25,650,"For service at:"),
    moveAndShow(PDF, 25,640,"7006 SUNCREST DR"),
    moveAndShow(PDF, 25,630,"SALINE MI 48176-9102"),
    
    eg_pdf:set_font(PDF,"Helvetica", 16),
    moveAndShow(PDF, 25,595,"News from Comcast"),  

    %% zap(PDF, Sample, X, Y, Measure, PtSize, Leading, NLines, Justification)
    PtSize10 = 10,

    TagMap10 = eg_xml2richText:default_tagMap(PtSize10),

    eg_block:block(PDF, xml(news), 25, 585, 240, PtSize10, 12, 20, ragged, TagMap10),
    %% eg_test3:zap(PDF, xml(news), 25, 585, 40, 10, 12, 20, ragged),

    eg_pdf:set_font(PDF,"Helvetica-Bold", 10),
    moveAndShow(PDF, 325,610,"Total Amount Due"),
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "$99.95"),610,"$99.95"),   

    eg_pdf:set_font(PDF,"Helvetica", 11),
    moveAndShow(PDF, 325,660,"Previous Balance"),   
    moveAndShow(PDF, 325,642,"Payment - 02/16/10 - thank you"),  
    moveAndShow(PDF, 325,627,"New Charges - see below"),  
    moveAndShow(PDF, 325,595,"Payment Due By"),  
    moveAndShow(PDF, 340,545,"Comcast High-Speed Internet"),  
    moveAndShow(PDF, 325,525,"Total New Charges"),  
                            
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "99.95"),660, "99.95"),
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "-99.95"),642,"-99.95"),     
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "99.95"),627,"99.95"),     
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "03/22/10"),595,"03/22/10"),     
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "99.95"),545,"99.95"),
    moveAndShow(PDF, 565 - eg_pdf:get_string_width(PDF, "Helvetica", 12, "$99.95"),525,"$99.95"),  
    
    eg_pdf:set_fill_gray(PDF, 1.0),
    eg_pdf:set_font(PDF,"Helvetica", 14),
    moveAndShow(PDF, 445 - round(eg_pdf:get_string_width(PDF, "Helvetica", 14, "Thank you for being a")/2),495,"Thank you for being a"), 
    moveAndShow(PDF, 445 - round(eg_pdf:get_string_width(PDF, "Helvetica", 14, "valued Comcast customer!")/2),480,"valued Comcast customer!"),           
 
    eg_pdf:set_fill_gray(PDF, 0.0),
    eg_pdf:set_font(PDF,"Helvetica", 8),
    moveAndShowRot(PDF, 585, 500, "016604 1/1", 90),
    
    
    eg_pdf:set_dash(PDF, dot),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:set_line_width(PDF,1),
    eg_pdf:line(PDF, 25,255,570,255),   
    
    CouponLine = "Detach and enclose this coupon with your payment. Please write your account number on your check or money order. Do not send cash.",
    moveAndShow(PDF, 277 - round(eg_pdf:get_string_width(PDF, "Helvetica", 8, CouponLine)/2),240,CouponLine),      

    eg_pdf:image(PDF, "../testing/comcast_logo.jpg",{50,200},{height,20}),
    
    eg_pdf:set_font(PDF,"Helvetica", 8),    
    moveAndShow(PDF, 80,180,"27800 FRANKLIN RD"),  
    moveAndShow(PDF, 80,170,"SOUTHFIELD MI 48034-2363"),
    
    moveAndShow(PDF, 55,150,"------- manifest line -------"),
    moveAndShow(PDF, 55,130,"SERVICE LEVEL CORP"),
    moveAndShow(PDF, 55,120,"7006 SUNCREST DRIVE"),
    moveAndShow(PDF, 55,110,"SALINE MI 48176-9102"),
    
    eg_pdf:set_font(PDF,"Helvetica-Bold", 10),
    moveAndShow(PDF, 330,220,"Account Number"),
    moveAndShow(PDF, 450,220,"09588 234102-01-7"),    
    moveAndShow(PDF, 330,200,"Payment Due by"),
    moveAndShow(PDF, 450,200,"04/15/10"),    
    moveAndShow(PDF, 330,180,"Total Amount Due"),
    moveAndShow(PDF, 450,180,"$99.95"),    
    moveAndShow(PDF, 330,150,"Amount Enclosed"),
    moveAndShow(PDF, 450,150,"$"),    
    
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:set_line_width(PDF,1),
    eg_pdf:line(PDF, 330,195,550,195),
    eg_pdf:line(PDF, 330,175,550,175), 
    eg_pdf:line(PDF, 455,145,550,145),   
    
    eg_pdf:set_font(PDF,"Helvetica", 10),
    moveAndShow(PDF, 330, 130,"Make checks payable to Comcast"), 
    moveAndShow(PDF, 350,90,"COMCAST"),
    moveAndShow(PDF, 350,81,"PO BOX 3005"),
    moveAndShow(PDF, 350,72,"SOUTHEASTERN PA"), 
    moveAndShow(PDF, 350,63,"19398-3005"), 
    
    eg_pdf:set_font(PDF,"OCR-A-Digits", 10),
    moveAndShow(PDF, 280,15,"09588   234102 01 7         0       013135"),
    
    eg_pdf:new_page(PDF),
    

    eg_pdf:set_dash(PDF, [1]),
    eg_pdf:set_stroke_gray(PDF, 0.5), %% black
    eg_pdf:set_line_width(PDF,1),
    eg_pdf:round_rect(PDF, {360,705},{210,75}, 5),
    eg_pdf:path(PDF, stroke),
    eg_pdf:set_font(PDF,"Helvetica", 10),
    Base = 760,
    Increment = 12,
    moveAndShow(PDF, 370,Base,                    "Account Number"),
    moveAndShow(PDF, 370,Base - Increment,        "Billing Date"),
    moveAndShow(PDF, 370,Base - (2 * Increment),  "Total Amount Due"),
    moveAndShow(PDF, 370,Base - (3 * Increment),  "Payment Due by"),
    moveAndShow(PDF, 470,Base,                    "09588 355496-01-5"),
    moveAndShow(PDF, 470,Base - Increment,        "02/28/10"),
    moveAndShow(PDF, 470,Base - (2 * Increment),  "$99.95"),
    moveAndShow(PDF, 470,Base - (3 * Increment),  "Page 2 of 2"),  
    
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF,dodgerblue),
    eg_pdf:set_line_width(PDF,2),
    eg_pdf:line(PDF, 25,700,570,700), 
    
    eg_pdf:image(PDF, "../testing/comcast_logo.jpg",{25,760},{height,20}),

    eg_pdf:set_fill_gray(PDF, 0.0),
    eg_pdf:set_font(PDF,"Helvetica-Bold", 10),
    moveAndShow(PDF, 30, 705,  "Contact us:"),  
    moveAndShow(PDF, 110,705,  "www.comcast.com"),  
    moveAndShow(PDF, 220,705,  "1-800-391-3000"),  
    
    moveAndShow(PDF, 25,735,"Service Details"),
    
    eg_pdf:image(PDF, "../testing/HighSpeedInternet.jpg",{25,665},{height,25}),    
    
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,orange),
    eg_pdf:round_rect(PDF, {50,665},{250,20}, 10),
    eg_pdf:path(PDF, fill),
    eg_pdf:restore_state(PDF), 
    
    eg_pdf:set_fill_gray(PDF, 1.0),
    eg_pdf:set_font(PDF,"Helvetica", 12),
    moveAndShow(PDF, 55,670,"Comcast High-Speed Internet"),
    eg_pdf:set_fill_gray(PDF, 0.0),
    
    eg_pdf:set_font(PDF,"Helvetica", 10),    
    moveAndShow(PDF, 25,650,  "Internet Preferred"),  
    moveAndShow(PDF, 40,640,  "Internet Preferred with"),     
    moveAndShow(PDF, 40,630,  "Microsoft Communication"),     
    moveAndShow(PDF, 40,620,  "Services,"),     
    moveAndShow(PDF, 40,610,  "4 web access E-mailboxes,"),     
    moveAndShow(PDF, 40,600,  "domain name,"),     
    moveAndShow(PDF, 40,590,  "starter website,"),
    moveAndShow(PDF, 25,560,  "CCO Static IP"),
    moveAndShow(PDF, 25,545,  "PRO Modem"),    
    
    moveAndShow(PDF, 180,650,  "03/08 - 04/07"), 
    moveAndShow(PDF, 180,560,  "03/08 - 04/07"), 
    moveAndShow(PDF, 180,545,  "03/08 - 04/07"), 
    
    moveAndShow(PDF, 300 - eg_pdf:get_string_width(PDF, "Helvetica", 10, "89.95"),650, "89.95"),
    moveAndShow(PDF, 300 - eg_pdf:get_string_width(PDF, "Helvetica", 10, "10.00"),560, "10.00"),
    moveAndShow(PDF, 300 - eg_pdf:get_string_width(PDF, "Helvetica", 10, "0.00"),545, "0.00"),

    eg_pdf:set_font(PDF,"Helvetica-Bold", 10),
    moveAndShow(PDF, 25,525,  "Total Comcast High-Speed Internet"),  
    moveAndShow(PDF, 300 - eg_pdf:get_string_width(PDF, "Helvetica-Bold", 10, "$99.95"),545, "$99.95"),

    eg_pdf:set_dash(PDF, dot),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:set_line_width(PDF,1),
    eg_pdf:line(PDF, 25,570,300,570),
    eg_pdf:line(PDF, 25,555,300,555), 
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:line(PDF, 25,540,300,540),   

%% zap(PDF, Sample, X, Y, Measure, PtSize, Leading, NLines, Justification)

%%    eg_test3:zap(PDF, xml(online), 25, 235, 40, 9, 12, 20, ragged),
    
%%    eg_test3:zap(PDF, xml(caption), 310, 235, 40, 9, 12, 20, ragged),    
    PtSize9 = 9,

    TagMap9 = eg_xml2richText:default_tagMap(PtSize9),


    eg_block:block(PDF, xml(online), 25, 235, 240, PtSize9, 12, 20, ragged, TagMap9),
    
    eg_block:block(PDF, xml(caption), 310, 235, 240, PtSize9, 12, 20, ragged, TagMap9),    
                                                          
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    ok = file:write_file("../testing/comcast_bill.pdf",[Serialised]),
    eg_pdf:delete(PDF),
    Stop = now(),
    io:format("Duration ~p  microseconds~n",[timer:now_diff( Stop, Start)]),
    bill_output.



xml(news) ->
  "<p><hb>IMPORTANT INFORMATION</hb><helv> - Our network enhancement is complete! </helv></p>
  
<p><helv> On 4/21/2010, any TV, including QAN tuner TVx, w\/o a Comcast
Digital device will only be able to receive Limited Basic channels 2-24, 95, 
96 and 99. Just visit us online at comcast.com/digitnow or call us at 1-877-634-4434
to get your equipment today.</helv></p><p><helv> All of us at Comcast extend our apreciation and thanks for being our 
customer, your opinion counts!!</helv></p> <p><helv>Please let us know any way we may better 
serve your needs.</helv></p>";

xml(online) ->
  "<p><hb>Pay On-Line Using Your Credit Card Or From Your Bank Account:</hb></p>
  <p><helv>Pay your bill online with any major credit card or from your bank account at comcast.com. 
  Simply click on \"Pay Your Bill Online\" from the comcast.com  homepage.</helv></p>
  <p>  </p>
  <p><hb>Automatic Bill Payments:</hb></p>
  <p><helv>Sign up for Comcast PayDirect and have your Comcast bill automatically paid from your bank account
  or charged to a credit card, on-time, every time. No more late payments or having to make a call.
  Click on \"Pay Your Bill Online\" from the comcast.com home page to sign up easily on-line.</helv></p>";
  
xml(caption) ->
    "<p><helv>Closed Captioning: For immediate assistance call: (800)266-2278, fax (215)286-4700 or go online
     for email or live chat at www.comcast.com/support.
    For written complaints contact: Frank Eliason, Comcast Closed Captioning Office, 1701 
    John F. Kennedy Blvd., Phila., PA 19103-2838, Captioning_Complaints@Comcast.com, fax:
    (215) 286-4700 or call:(215) 286-4697.</helv></p>
    <p>  </p>
    <p><hb>Franchise Authority Information:</hb></p>
    <p><helv>Pittsfield Township (MI0636)</helv></p>
    <p><helv>6201 W. Michigan Ave</helv></p>
    <p><helv>Ann Arbor MI, 48108</helv></p>".







