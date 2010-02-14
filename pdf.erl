%%======================================================================
%%  PDF interface - see erlguten_test1.pdf for example usage
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Joe Armstrong, Mikael Karlsson 
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
%%            Mikael Karlsson <mikael.karlsson@creado.com>
%% Last Edit: 2003-03-11
%% =====================================================================

-module(pdf).

%% Purpose: Generate PDF documents main api

-include("erlguten.hrl").

-compile(export_all).

%% -export([new/0]).


-import(erlguten_pdf_assemble, 
	[build_pdf/4, builtin_pdf_fonts/0, serialise2bin/1,
	 pdfloop/2]).

%% Set up Info, Catalog and Pages

init_pdf_context()->
    #pdfContext{info=#info{creator="Erlang", 
			   creationDate="20030215152011",
			   producer="erlguten-2.0", 
			   author="",
			   title="",
			   subject="",
			   keywords="ErlangKeyword"},
		images=dict:new(),
		fonts=dict:new(),
		currentpage=1,
		mediabox={0,0,595,842}}.

%% --------------------- User functions --------------

new()->
    erlguten_font_server:start(),
    spawn_link(fun() -> pdfloop(init_pdf_context(), []) end).

%% Export to PDF file format 

export(PID)->
    PID ! {export, self()},
    receive
	{export,PDF}->
	    PDF
    end.

%% clear up

delete(PID)->
    PID ! delete,
    done.


%% Add current page context to PDF document
%% and start on a new page 

new_page(PID)->
    PID ! {page, {new, self()}},
    receive
	{page,PageNo}->
	    PageNo
    end.
    
set_page(PID, PageNo)->
    PID ! {page,{set, PageNo}}.

get_page_no(PID)->
    PID ! {page, {get_no, self()}},
    receive
	{page,PageNo}->
	    PageNo
    end.
    
%% --- Info -----

set_author(PDF,Author)->
    PDF ! {info,{author, Author}}.

set_title(PDF,Title)->
    PDF ! {info,{title, Title}}.

set_subject(PDF,Subject)->
    PDF ! {info,{subject, Subject}}.

set_date(PDF,Year,Month,Day)->
    PDF ! {info,{date, {Year,Month,Day}}}.

set_keywords(PDF, Keywords)->
    PDF ! {info,{keywords, Keywords}}.

%% --- Page ---

set_pagesize(PDF,a4)             -> set_pagesize(PDF, 595.27, 841.89 ).
set_pagesize(PDF, Width, Height) -> PDF ! {mediabox,{0,0,Width,Height}}.

%% -- Fonts --

set_font(PID, Fontname, Size)->
    PID ! {font, {set, Fontname, Size}}.

get_font_alias(PID, FontName) ->
    PID ! {get_font_alias, self(), FontName},
    receive
	{Pid, font_alias, I} ->
	    I
    end.

%% Text

begin_text(PDF)-> append_stream(PDF, "\nBT\n").
end_text(PDF)  -> append_stream(PDF, "\nET\n").

break_text(PDF)->
    append_stream(PDF, " T*\n").
    
text(PDF, Text) -> 
    A = escapePdfText(Text),
    B = ["(",A,")"," Tj\n"],
    append_stream(PDF, B).

textbr(PDF,Text)->
    text(PDF, Text),
    break_text(PDF).

kernedtext(PDF, Text)->
    append_stream(PDF, ["[ ",kernedtext(Text)," ] TJ\n"]).

kernedtext([]) ->[];
kernedtext([H|T]) when list(H)->  
    A = escapePdfText(H),
    ["(",A,") ",kernedtext(T)];
kernedtext([H|T]) when integer(H) ->
    [i2s(H)," ",kernedtext(T)].

set_text_pos(PDF, X, Y)->
    append_stream(PDF, [i2s(X)," ",i2s(Y)," Td "]).
set_text_leading(PDF, L)->
    append_stream( PDF, [n2s(L)," TL "]).

set_text_rendering(PDF, MODE) when integer(MODE)->
    append_stream(PDF, [i2s(MODE)," Tr\n"]);
set_text_rendering(PDF, fill)->
    set_text_rendering(PDF,0);
set_text_rendering(PDF, stroke) ->
    set_text_rendering(PDF,1);
set_text_rendering(PDF, fill_then_stroke) ->
    set_text_rendering(PDF,2).

set_char_space(PDF, CS) -> append_stream(PDF, [i2s(CS)," Tc "]).
set_word_space(PDF, WS) -> append_stream(PDF, [i2s(WS)," Tw "]).
set_text_scale(PDF, SC) -> append_stream(PDF, [i2s(SC)," Tz "]).
set_text_rise(PDF, RISE)-> append_stream(PDF, [i2s(RISE)," Ts "]).


%% Graphics operators
path(PDF,close)->
    append_stream(PDF," h ");
path(PDF,stroke)->
    append_stream(PDF," S ");
path(PDF,close_stroke)->
    append_stream(PDF," s ");
path(PDF,fill) ->
    append_stream(PDF," f ");
path(PDF,fill_even_odd) ->
    append_stream(PDF," f* ");
path(PDF,fill_stroke) ->
    append_stream(PDF," B ");
path(PDF,fill_stroke_even_odd) ->
    append_stream(PDF," B* ");
path(PDF,close_fill_stroke) ->
    append_stream(PDF," b ");
path(PDF,close_fill_stroke_even_odd) ->
    append_stream(PDF," b* ");
path(PDF, endpath) ->
    append_stream(PDF," n ").

moveto(PDF,{X,Y})->    
    [n2s([X,Y])," m "].

line(PDF,{Point1,Point2})->
    line(PDF,{Point1},{Point2}).
line(PDF,{X1,Y1},{X2, Y2})->
    line(PDF,X1,Y1,X2,Y2).
line(PDF,X1,Y1,X2,Y2)->
    append_stream(PDF, [n2s([X1,Y1])," m ",n2s([X2,Y2])," l S\n"]).

lines(PDF,[])->
    [];
lines(PDF,[H|T])->
    line(PDF,H),
    lines(PDF,T).

poly(PDF,[])->
    [];
poly(PDF,[{X1,Y1}|PolyList])->
    append_stream(PDF,[n2s([X1,Y1])," m " |poly1(PolyList)]).
poly1([])->
    "\n";
poly1([{X1,Y1}|T]) ->
    [n2s([X1,Y1])," l " |poly1(T)].

grid(PDF,XList,YList)->
    tobedone.


%% Bezier paths should be stroked/closed filled with separate
%% command?
bezier(PDF,{Point1,Point2,Point3,Point4})->
    bezier(PDF,Point1,Point2,Point3,Point4).
bezier(PDF,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4})->
    bezier(PDF,X1,Y1,X2,Y2,X3,Y3,X4,Y4).
bezier(PDF,X1,Y1,X2,Y2,X3,Y3,X4,Y4)->
    append_stream(PDF, [n2s([X1,Y1])," m "]),
    bezier_c(PDF,{X2,Y2},{X3,Y3},{X4,Y4}).
bezier_c(PDF,{X1,Y1},{X2,Y2},{X3,Y3})->
    append_stream(PDF, [n2s([X1,Y1,X2,Y2,X3,Y3]), " c "]).
bezier_v(PDF,{X2,Y2},{X3,Y3})->
    append_stream(PDF, [n2s([X2,Y2,X3,Y3]), " v "]).
bezier_y(PDF,{X1,Y1},{X3,Y3})->
    append_stream(PDF, [n2s([X1,Y1,X3,Y3]), " y "]).
    
arc(PDF,X1,Y1,X2,Y2)->
    tobedone.

rectangle(PDF,X,Y,WX,WY) ->
    rectangle(PDF,X,Y,WX,WY,stroke).
rectangle(PDF,X,Y,WX,WY,Option)->
    append_stream(PDF,rectangle1(X,Y,WX,WY,Option)).

rectangle1(X,Y,WX,WY,stroke)->
    [rectangle1(X,Y,WX,WY), " S\n"];
rectangle1(X,Y,WX,WY,fill)->
    [rectangle1(X,Y,WX,WY), " f\n"];
rectangle1(X,Y,WX,WY,fill_then_stroke)->
    [rectangle1(X,Y,WX,WY), " B\n"].
rectangle1(X,Y,WX,WY)->
    [n2s([X,Y,WX,WY])," re"].

%% Line styles
set_line_width(PDF,W)->
    append_stream(PDF, [n2s(W)," w\n"]).

set_line_cap(PDF,Mode)->
    append_stream(PDF, [n2s(Mode)," J\n"]).    

set_line_join(PDF,Mode)->
    append_stream(PDF, [n2s(Mode)," j\n"]).    

set_miter_limit(PDF,Limit)->
    append_stream(PDF, [n2s(Limit)," M\n"]).    

set_dash(PDF)->
    set_dash(PDF,[],0).

set_dash(PDF,Array,Phase)->
    append_stream(PDF,["[",n2s(Array),"] ",n2s(Phase)," d\n"]).

%% Graphics state
save_state(PDF)->
    append_stream(PDF, "\nq\n").
restore_state(PDF)->
    append_stream(PDF, "\nQ\n").

%% Change geometry
transform(PDF, A, B, C, D, E, F)->
    append_stream(PDF,[n2s([A,B,C,D,E,F])," cm\n"]).

translate(PDF, X, Y)->
    transform(PDF,1,0,0,1,X,Y).

scale(PDF, ScaleX, ScaleY) when integer(ScaleX),integer(ScaleY)->
    transform(PDF, ScaleX, 0, 0, ScaleY, 0, 0).

rotate(PDF, 90)->
    append_stream(PDF, " 0 1 -1 0 0 0 cm\n");
rotate(PDF, 180)->
    append_stream(PDF, " 1 0 0 1 0 0 cm\n");
rotate(PDF, 270)->
    append_stream(PDF, " 0 -1 1 0 0 0 cm\n");
rotate(PDF, Angle)->
    RadianAngle = Angle * math:pi()/180, 
    C = math:cos( RadianAngle ),
    S = math:sin( RadianAngle ),
    transform(PDF, C, S, -S, C, 0, 0).

skew(PDF, XScewAngle, YScewAngle)->
    TanA = math:tan(XScewAngle * math:pi()/180),
    TanB = math:tan(YScewAngle * math:pi()/180),
    transform(PDF, 1, TanA, TanB, 1, 0, 0).

mirror_yaxis(PDF,Xtranslate)->
    translate(PDF,Xtranslate,0),
    scale(PDF,-1,1).
mirror_xaxis(PDF,Ytranslate)->
    translate(PDF,0,Ytranslate),
    scale(PDF,1,-1).

%% Changing colors
set_fill_color_CMYK(PDF,C,M,Y,K)->
    append_stream(PDF, [n2s([C,M,Y,K])," k\n"]).

set_stroke_color_CMYK(PDF,C,M,Y,K)->
    append_stream(PDF, [n2s([C,M,Y,K])," K\n"]).

set_fill_color_RGB(PDF,R,G,B)->
    append_stream(PDF, [n2s([R,G,B])," rg\n"]).

set_stroke_color_RGB(PDF,R,G,B)->
    append_stream(PDF, [n2s([R,G,B]), " RG\n"]).

%% set_fill_color(PDF, ColorName)->
%%    tobedone.
%% set_stroke_color(PDF, ColorName)->
%%     tobedone.

%% Gray 0.0-Black 1.0-White)
set_fill_gray(PDF, Gray)->
    append_stream(PDF, [n2s(Gray)," g\n"]).
set_stroke_gray(PDF, Gray)->
    append_stream(PDF, [n2s(Gray)," G\n"]).
    
%% Images
image(PDF, "http:" ++ URL)->
    tobedone;
image(PDF, FilePath)->
    PDF ! {image,FilePath}.


%% Internals
append_stream(PDF, String)->
	PDF ! {stream, {append, String}}.

escapePdfText([]) -> [];
escapePdfText([$(|Rest]) -> [$\\,$( | escapePdfText(Rest)];
escapePdfText([$)|Rest]) -> [$\\,$) | escapePdfText(Rest)];
escapePdfText([$\\|Rest]) -> [$\\,$\\ | escapePdfText(Rest)];
escapePdfText([C|Rest]) -> [ C | escapePdfText(Rest)].

i2s(I) ->
    integer_to_list(I).

n2s(A) when float(A)   -> f2s(A);
n2s(A) when integer(A) -> i2s(A);
n2s([])                -> [];
n2s([H|T])             -> [n2s(H)," "|n2s(T)].

f2s(I) when integer(I) ->
    i2s(I);
f2s(F) ->    
    remove_leading_blanks(flatten(io_lib:format("~8.2f", [F]))).

remove_leading_blanks([$\s|T]) -> remove_leading_blanks(T);
remove_leading_blanks(X)       -> X.

flatten(L) ->
    binary_to_list(list_to_binary(L)).








