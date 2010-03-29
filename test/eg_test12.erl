%%======================================================================
%% Purpose: Test PDF documents main api
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Mikael Karlsson
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
%% Authors:   Mikael Karlsson <mikael.karlsson@creado.com>
%% Last Edit: 2003-04-04
%% =====================================================================

-module(eg_test12).

%% Date:    2004-08-03
%% Purpose: Test PDF documents main api

-export([test/0]).

test()->
    Info = [{"Author", "Mikael Karlsson"},
	    {"Title", "Test of PDF API"},
	    {"Subject", "PDF is cool, and with Erlang it is even cooler"},
	    {"Keywords", "Erlang, PDF, Gutenberg"}],

    Fonts = ["Times-Roman", "Times-Italic", "Courier", "Helvetica-Bold"],

    PageContents = [page_contents(1),page_contents(2),page_contents(3), page_contents(4)], 

    MediaBox = {rect,pdf:pagesize(a4)}, %% {rect,{0,0,595,842}}
    

    Images = [],
    {Free0,XObjectsPtr,OOs}  = eg_pdf_image:mk_images(Images, 1, [], []),
%%    OOs = [],
    {FontsPtr, O1s} = eg_pdf_obj:fonts(Fonts, OOs),
    
    ProcSet = undefined,
    KidRefs = [], %% No pages in the page tree yet in this example

    PTree = eg_pdf_page:page_tree(KidRefs, FontsPtr, XObjectsPtr, 
				  MediaBox, ProcSet),

    {PagesRef, O2s} = eg_pdf_lib:add_object(PTree, O1s),

    O3s = lists:foldl(fun(PageContent, Accu1) -> 
			      eg_pdf_page:append_page(PageContent, Accu1)
		      end, 
		      O2s, PageContents),
    
    {RootRef,O4} = eg_pdf_lib:add_object(
		     eg_pdf_obj:catalogue(PagesRef, []),
		     O3s),

    {InfoRef, O5} = eg_pdf_lib:add_object(eg_pdf_obj:info(Info), O4), 

    O6 = add_link({100,795,510,810},1, 2, O5),

    FinalObjects = number_pages(4, 1, O6),

    ReadyToWrite = eg_pdf_lib:export(InfoRef, FinalObjects),
    file:write_file("../test/eg_test12.pdf",[ReadyToWrite]).

add_link({X0,Y0,X1,Y1}=R, FPNo, TPNo, Objects) ->
    Destination  = eg_pdf_obj:destination( eg_pdf_lib:get_ref(eg_pdf_page:get_page(TPNo,Objects)) ,{"XYZ",0,792,0}),
    AnnotRef = eg_pdf_lib:get_next_ref(Objects),
    Annot = eg_pdf_lib:make_object(AnnotRef, eg_pdf_annot:new_link({rect,R},Destination)),
%%    Annot = eg_pdf_lib:make_object(AnnotRef, eg_pdf_annot:new_text({rect,R},"Hi There, this is a text annotation")),
    Page1 = eg_pdf_page:get_page(FPNo,Objects),
    Page1Dict = eg_pdf_lib:pdf_item(Page1),
    Page11= eg_pdf_lib:make_object(eg_pdf_lib:get_ref(Page1),
			eg_pdf_lib:store_in_dict({"Annots",{array,[{ptr,AnnotRef,0}]}}, Page1Dict)
		       ),
    eg_pdf_lib:store_object([Page11, Annot],Objects).

%% It would of course be simpler to just add pageno to the contents streams
%% PageContents directly, but this is just to test that the right page
%% is retreived from Objects and that new text can be added to its content
%% stream.
number_pages(NoOfPages, PageNo, Objects) when (PageNo > NoOfPages) ->
    Objects;
number_pages(NoOfPages, PageNo, Objects) ->
    PageObj = eg_pdf_page:get_page(PageNo, Objects),
    NewObjects = eg_pdf_page:append_to_page( pageno(PageNo), PageObj, Objects), 
    number_pages(NoOfPages, PageNo+1, NewObjects). 

page_contents(1)-> 
     [ draft(),
       pdf_op:save_state(),
%% Images not impl in pdf_op yet
%%       pdf_op:image('joenew.jpg',{390,440},{height,140}),
%%       pdf_op:image('joenew.jpg',{390,200},{width,140}),
%%       pdf_op:image('joenew.jpg',{190,300},{140,140}),
       pdf_op:begin_text(),
       pdf_op:set_font( "Times-Italic", 240),
       pdf_op:set_text_pos(60,600),
       pdf_op:textbr("Wow"),
       pdf_op:set_font("Times-Roman", 42),
       pdf_op:set_text_pos(60,-40),
       pdf_op:set_text_leading(60),
       pdf_op:text("Welcome home "),
       pdf_op:set_text_rise(20),
       pdf_op:textbr("Joe, "),
       pdf_op:set_text_rise(0),
       pdf_op:textbr("hope you had a"),
       pdf_op:textbr("nice trip"),
       pdf_op:end_text(),
       pdf_op:restore_state()
      ];

page_contents(2) ->
    [ draft(),
      pdf_op:save_state(),
      pdf_op:begin_text(),
      pdf_op:set_font("Times-Roman", 24),
      pdf_op:set_text_pos(60,750),
      pdf_op:set_text_leading(26),
      pdf_op:textbr("Times-Roman 24 pt"),
      pdf_op:set_font("Times-Italic", 16),
      pdf_op:textbr("Times-Italic 16 pt"),
      pdf_op:set_font("Courier", 6),
      pdf_op:textbr("Courier 6 pt"),
      pdf_op:set_text_leading(14),
      pdf_op:set_font("Blahonga", 12),
      pdf_op:textbr("The Blahonga font will fall back to Times-Roman"),
      pdf_op:textbr("This is a check of ( ) \\ escape chars"),
      pdf_op:kernedtext( 
	[ "This is a test of Kerning: A", 120, "W", 
	  120, "A", 95, "Y again" ]),
      pdf_op:break_text(),
      pdf_op:textbr("This is a text without Kerning: AWAY again"),
      pdf_op:break_text(),
      pdf_op:end_text(),
      pdf_op:restore_state()
     ];

page_contents(3) ->
    [
    pdf_op:save_state(),
    pdf_op:line(100,100,200,100),
    pdf_op:bezier(100,100,100,200,200,200,200,100),
    pdf_op:path(stroke),
    pdf_op:set_fill_color_RGB(0.5,0.7,0.2),
    pdf_op:bezier(300,100,300,200,500,200,500,100),
    pdf_op:path(close_fill_stroke),
    pdf_op:set_fill_color(purple),
    pdf_op:bezier({300,400},{300,450},{500,450},{500,400}),
    pdf_op:bezier_c({500,350},{400,350},{300,400}),
    pdf_op:path(fill),
    pdf_op:set_dash(dash),
    pdf_op:set_fill_color(slateblue),
    pdf_op:line(100,250,400,250),
    pdf_op:poly([{100,300},{150,350},{200,350},{250,300}]),
    pdf_op:path(fill_stroke),
    pdf_op:set_dash(solid),
    pdf_op:set_stroke_color(khaki),
    pdf_op:circle({200,200}, 200),
    pdf_op:path( stroke),
    pdf_op:set_stroke_color({16#00,16#FF,16#00}),
    pdf_op:circle({200,300}, 50),
    pdf_op:path(stroke),
    pdf_op:ellipse( {200,300}, {50,100}),
    pdf_op:path(stroke),
    pdf_op:ellipse( {200,300}, {100,50}),
    pdf_op:path( stroke),
    pdf_op:circle( {200,300}, 100),
    pdf_op:path( stroke),
    pdf_op:grid([50,100,150],[600,700,800]),
    pdf_op:round_rect({300,600},{200,100},20),
    pdf_op:path( stroke),
     pdf_op:rectangle({300,600},{200,100}, stroke),
     pdf_op:restore_state()
    ];

page_contents(4) ->
    colortest().


%% Write a DRAFT text rotated in the background
draft()->
    [ pdf_op:save_state(),
      pdf_op:set_fill_gray(0.75),
      pdf_op:rectangle(100,800,410,5, fill),
      pdf_op:rectangle(100,42,410,5,fill_then_stroke),
      pdf_op:translate(150,650),
      pdf_op:mirror_yaxis(300),
      pdf_op:rotate(300),
      pdf_op:begin_text(),
      pdf_op:set_font("Helvetica-Bold", 160),
      pdf_op:set_text_rendering(fill),
      pdf_op:set_text_pos(0,0),
      pdf_op:textbr("DRAFT"),
      pdf_op:end_text(),
      pdf_op:restore_state()
     ].

pageno(PageNo)->
    Str = "Page " ++ pdf_op:n2s(PageNo),
    Width = pdf_op:get_string_width("Times-Roman", 11, Str),
    TextPos = case PageNo rem 2 of
		  0 ->
		      pdf_op:set_text_pos(100, 50);
		  1 ->
		      pdf_op:set_text_pos(510 - Width, 50)
	      end,
    S = [ pdf_op:save_state(),
	  pdf_op:begin_text(),
	  pdf_op:set_font("Times-Roman", 11),
	  TextPos,
	  pdf_op:text(Str),
	  pdf_op:end_text(),
	 pdf_op:restore_state()].


colortest()->
    D=50,S=750, M=60,
    [
     pdf_op:save_state(),
     pdf_op:translate(M, S),
     colortest1(0,[white,silver,gray,black,maroon,red,fuchsia,purple,lime,
		   green,olive,yellow,navy,blue,teal,aqua]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-D),
     colortest1(0,[blue2,blue3,blue4,blueviolet,cornflowerblue,darkorchid,
		   darkslateblue,dodgerblue,lightskyblue,mediumblue, 
		   mediumpurple, mediumslateblue,midnightblue,purpleblue,
		   royalblue,skyblue2, slateblue]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-2*D),
     colortest1(0,[aquamarine4,cadetblue,darkturquoise,lightblue,
		   lightseagreen,lightslategray,lightsteelblue,
		   mediumturquoise,paleturquoise,powderblue,skyblue,
		   steelblue,turquoise]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-3*D),
     colortest1(0,[antiquewhite3,antiquewhite4,azure3, beige,darkslategray,
		   gainsboro,honeydew,slategray,thistle
		  ]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-4*D),
     colortest1(0,[aquamarine,chartreuse,darkgreen,darkseagreen,forestgreen,
		   green2,green3,green4,greenyellow,lawngreen,limegreen,
		   mediumaquamarine,mediumseagreen,mediumspringgreen,
		   olivedrab,palegreen]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-5*D),
     colortest1(0,[seagreen,springgreen,yellowgreen]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-6*D),
     colortest1(0,[magenta,magenta2,magenta3,magenta4,mediumorchid,orchid,
		   plum,violet]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-7*D),
     colortest1(0,[brown,burlywood,chocolate,coral,darkgoldenrod,darkorange,
		   darksalmon,deeppink,firebrick,gold,goldenrod,hotpink,
		   indianred,lightcoral,lightpink,lightsalmon]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-8*D),
     colortest1(0,[maroon0,orange,orangered,palevioletred,peachpuff,peru,
		   pink,red2,red3,red4,rosybrown,salmon,sandybrown,sienna,
		   tomato,violetred]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-9*D),
     colortest1(0,[aliceblue,azure,floralwhite,ghostwhite,ivory,lavender,
		   lavenderblush,lightcyan,lightyellow,linen,mintcream,
		   mistyrose,oldlace,seashell,snow,whitesmoke]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-10*D),
     colortest1(0,[antiquewhite,bisque,blancedalmond,comsilk,darkkhaki,
		   darkolivegreen,khaki,lemonchiffon,lightgoldenrod,
		   lightgoldenrodyellow]),
     pdf_op:restore_state(),
     pdf_op:save_state(),
     pdf_op:translate(M, S-11*D),
     colortest1(0,[moccasin,palegoldenrod,papayawhip,
		   tan,wheat,yellow2,yellow3,yellow4]),
     
     pdf_op:restore_state()].

colortest1(N,[])->
    [];
colortest1(N,[H|T])->
    [ pdf_op:set_fill_color(H),
      pdf_op:rectangle({0,20},{20,20}),
      pdf_op:path(fill_stroke),
      pdf_op:set_fill_color(black),
      pdf_op:begin_text(),
      pdf_op:set_font("Times-Roman", 8),
      pdf_op:set_text_pos(0,(N rem 2)*10),
      pdf_op:text(atom_to_list(H)),
      pdf_op:end_text(),
      pdf_op:translate(30,0) | colortest1(N+1,T)].
