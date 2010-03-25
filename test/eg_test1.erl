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

-module(eg_test1).

%% Date:    2003-02-26
%% Purpose: Test PDF documents main api

-export([test/0]).

test()->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,a4),
    pdf:set_author(PDF,"Mikael Karlsson"),
    pdf:set_title(PDF, "Test of PDF API"),
    pdf:set_subject(PDF,"PDF is cool, but Erlang is cooler"),
    pdf:set_keywords(PDF,"Erlang, PDF, Gutenberg"),
%%    pdf:set_date(PDF,2003,3,31),
    draft(PDF),
    pdf:new_page(PDF),
    draft(PDF),

    pdf:set_page(PDF,1),
    pdf:image(PDF,'../test/joenew.jpg',{390,440},{height,140}),
    pdf:image(PDF,'../test/joenew.jpg',{390,200},{width,140}),
    pdf:image(PDF,'../test/joenew.jpg',{190,300},{140,140}),
    pdf:begin_text(PDF),
    pdf:set_font(PDF, "Times-Italic", 240),
    pdf:set_text_pos(PDF, 60,600),
    pdf:textbr(PDF, "Wow"),
    pdf:set_font(PDF, "Times-Roman", 42),
    pdf:set_text_pos(PDF, 60,-40),
    pdf:set_text_leading(PDF,60),
    pdf:text(PDF, "Welcome home "),
    pdf:set_text_rise(PDF, 20),
    pdf:textbr(PDF, "Joe, "),
    pdf:set_text_rise(PDF, 0),
    pdf:textbr(PDF, "hope you had a"),
    pdf:textbr(PDF, "nice trip"),
    pdf:end_text(PDF),

    pdf:set_page(PDF,2),

    pdf:begin_text(PDF),
    pdf:set_font(PDF,"Times-Roman", 24),
    pdf:set_text_pos(PDF,60,750),
    pdf:set_text_leading(PDF,26),
    pdf:textbr(PDF, "Times-Roman 24 pt"),
    pdf:set_font(PDF, "Times-Italic", 16),
    pdf:textbr(PDF,"Times-Italic 16 pt"),
    pdf:set_font(PDF, "Courier", 6),
    pdf:textbr(PDF, "Courier 6 pt"),
    pdf:set_text_leading(PDF,14),
    pdf:set_font(PDF,"Blahonga", 12),
    pdf:textbr(PDF, "The blahonga font will fall back to Times-Roman"),
    pdf:textbr(PDF, "This is a check of ( ) \\ escape chars"),
    pdf:kernedtext(PDF, 
		   [ "This is a test of Kerning: A", 120, "W", 
		     120, "A", 95, "Y again" ]),
    pdf:break_text(PDF),
    pdf:textbr(PDF, "This is a text without Kerning: AWAY again"),
    pdf:break_text(PDF),
    pdf:end_text(PDF),

    pdf:new_page(PDF),
    pdf:line(PDF,100,100,200,100),
    pdf:bezier(PDF,100,100,100,200,200,200,200,100),
    pdf:path(PDF,stroke),
    pdf:set_fill_color_RGB(PDF,0.5,0.7,0.2),
    pdf:bezier(PDF,300,100,300,200,500,200,500,100),
    pdf:path(PDF,close_fill_stroke),
    pdf:set_fill_color(PDF,purple),
    pdf:bezier(PDF,{300,400},{300,450},{500,450},{500,400}),
    pdf:bezier_c(PDF,{500,350},{400,350},{300,400}),
    pdf:path(PDF,fill),
    pdf:set_dash(PDF,dash),
    pdf:set_fill_color(PDF,slateblue),
    pdf:line(PDF,100,250,400,250),
    pdf:poly(PDF,[{100,300},{150,350},{200,350},{250,300}]),
    pdf:path(PDF,fill_stroke),
    pdf:set_dash(PDF,solid),
    pdf:set_stroke_color(PDF,khaki),
    pdf:circle(PDF, {200,200}, 200),
    pdf:path(PDF, stroke),
    pdf:set_stroke_color(PDF, {16#00,16#FF,16#00}),
    pdf:circle(PDF, {200,300}, 50),
    pdf:path(PDF, stroke),
    pdf:ellipse(PDF, {200,300}, {50,100}),
    pdf:path(PDF, stroke),
    pdf:ellipse(PDF, {200,300}, {100,50}),
    pdf:path(PDF, stroke),
    pdf:circle(PDF, {200,300}, 100),
    pdf:path(PDF, stroke),
    pdf:grid(PDF,[50,100,150],[600,700,800]),
    pdf:round_rect(PDF,{300,600},{200,100},20),
    pdf:path(PDF, stroke),
    pdf:rectangle(PDF,{300,600},{200,100}, stroke),
    pdf:new_page(PDF),
    colortest(PDF),
    Serialised = pdf:export(PDF),
    file:write_file("eg_test1.pdf",[Serialised]),
    pdf:delete(PDF).

%% Write a DRAFT text rotated in the background
draft(PDF)->
    pdf:save_state(PDF),
    pageno(PDF),
    pdf:set_fill_gray(PDF,0.75),
    pdf:rectangle(PDF, 100,800,410,5, fill),
    pdf:rectangle(PDF, 100,42,410,5,fill_then_stroke),
    pdf:translate(PDF,150,650),
    pdf:mirror_yaxis(PDF,300),
    pdf:rotate(PDF,300),
    pdf:begin_text(PDF),
    pdf:set_font(PDF,"Helvetica-Bold", 160),
    pdf:set_text_rendering(PDF, fill),
    pdf:set_text_pos(PDF, 0,0),
    pdf:textbr(PDF, "DRAFT"),
    pdf:end_text(PDF),
    pdf:restore_state(PDF).

pageno(PDF)->
    pdf:begin_text(PDF),
    pdf:set_font(PDF,"Times-Roman", 11),
    A = pdf:get_page_no(PDF),
    Str = "Page " ++ pdf_op:n2s(A),
    Width = pdf:get_string_width(PDF,"Times-Roman", 11, Str),
    case A rem 2 of
	0 ->
	    pdf:set_text_pos(PDF, 100, 50);
	1 ->
	    pdf:set_text_pos(PDF, 510 - Width, 50)
    end,
    pdf:text(PDF, "Page " ++ pdf_op:n2s(A)),
    pdf:end_text(PDF).


colortest(PDF)->
    D=50,S=750, M=60,
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S),
    colortest1(PDF,0,[white,silver,gray,black,maroon,red,fuchsia,purple,lime,
		   green,olive,yellow,navy,blue,teal,aqua]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-D),
    colortest1(PDF,0,[blue2,blue3,blue4,blueviolet,cornflowerblue,darkorchid,
		      darkslateblue,dodgerblue,lightskyblue,mediumblue, 
		      mediumpurple, mediumslateblue,midnightblue,purpleblue,
		     royalblue,skyblue2, slateblue]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-2*D),
    colortest1(PDF,0,[aquamarine4,cadetblue,darkturquoise,lightblue,
		      lightseagreen,lightslategray,lightsteelblue,
		      mediumturquoise,paleturquoise,powderblue,skyblue,
		     steelblue,turquoise]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-3*D),
    colortest1(PDF,0,[antiquewhite3,antiquewhite4,azure3, beige,darkslategray,
		     gainsboro,honeydew,slategray,thistle
		     ]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-4*D),
    colortest1(PDF,0,[aquamarine,chartreuse,darkgreen,darkseagreen,forestgreen,
		     green2,green3,green4,greenyellow,lawngreen,limegreen,
		     mediumaquamarine,mediumseagreen,mediumspringgreen,
		     olivedrab,palegreen]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-5*D),
    colortest1(PDF,0,[seagreen,springgreen,yellowgreen]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-6*D),
    colortest1(PDF,0,[magenta,magenta2,magenta3,magenta4,mediumorchid,orchid,
		     plum,violet]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-7*D),
    colortest1(PDF,0,[brown,burlywood,chocolate,coral,darkgoldenrod,darkorange,
		      darksalmon,deeppink,firebrick,gold,goldenrod,hotpink,
		      indianred,lightcoral,lightpink,lightsalmon]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-8*D),
    colortest1(PDF,0,[maroon0,orange,orangered,palevioletred,peachpuff,peru,
		     pink,red2,red3,red4,rosybrown,salmon,sandybrown,sienna,
		     tomato,violetred]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-9*D),
    colortest1(PDF,0,[aliceblue,azure,floralwhite,ghostwhite,ivory,lavender,
		     lavenderblush,lightcyan,lightyellow,linen,mintcream,
		     mistyrose,oldlace,seashell,snow,whitesmoke]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-10*D),
    colortest1(PDF,0,[antiquewhite,bisque,blancedalmond,comsilk,darkkhaki,
		     darkolivegreen,khaki,lemonchiffon,lightgoldenrod,
		     lightgoldenrodyellow]),
    pdf:restore_state(PDF),
    pdf:save_state(PDF),
    pdf:translate(PDF, M, S-11*D),
    colortest1(PDF,0,[moccasin,palegoldenrod,papayawhip,
		     tan,wheat,yellow2,yellow3,yellow4]),

    pdf:restore_state(PDF).

colortest1(PDF,N,[])->
    [];
colortest1(PDF,N,[H|T])->
    pdf:set_fill_color(PDF,H),
    pdf:rectangle(PDF,{0,20},{20,20}),
    pdf:path(PDF,fill_stroke),
    pdf:set_fill_color(PDF,black),
    pdf:begin_text(PDF),
    pdf:set_font(PDF,"Times-Roman", 8),
    pdf:set_text_pos(PDF,0,(N rem 2)*10),
    pdf:text(PDF,atom_to_list(H)),
    pdf:end_text(PDF),
    pdf:translate(PDF,30,0),
    colortest1(PDF,N+1,T).
