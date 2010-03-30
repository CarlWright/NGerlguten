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
%% Last Edit: 2003-04-02
%% =====================================================================

-module(pdf).

%% Purpose: Generate PDF documents main api

-include("eg.hrl").

-export ([new/0, set_pagesize/2, set_author/2, set_title/2,set_subject/2,
  set_keywords/2,save_state/1,begin_text/1,set_font/3, get_page_no/1,inBuiltFonts/0,
  get_string_width/4, set_text_pos/3, text/2, end_text/1, set_fill_gray/2,
  rectangle/6, translate/3, mirror_yaxis/2,rotate/2, set_text_rendering/2,
  textbr/2, restore_state/1, new_page/1, set_page/2, image/4, set_text_leading/2,
  set_text_rise/2, kernedtext/2, break_text/1, line/5, bezier/9, path/2,
  set_fill_color_RGB/4, set_fill_color/2, bezier/5, bezier_c/4, set_dash/2,
  poly/2, set_stroke_color/2, circle/3, ellipse/3, grid/3, round_rect/4,
  rectangle/4, rectangle/3, export/1, delete/1, ensure_font_gets_loaded/2,
  append_stream/2, set_line_width/2, pagesize/1, page_script/2, set_date/4,
  set_pagesize/3, set_char_space/2, set_word_space/2, set_text_scale/2,
   move_to/2, line/3, lines/2, bezier_v/3, bezier_y/3,arc/5, set_line_cap/2,
   set_line_join/2, set_miter_limit/2, set_dash/3, transform/7, scale/3,
   skew/3,  mirror_xaxis/2, set_fill_color_CMYK/5, set_stroke_color_CMYK/5,
   set_stroke_color_RGB/4, set_stroke_gray/2, image/2, image/3, fontName/1,
   default_face/0, allFonts/0]).

%% -export([new/0]).


-import(eg_pdf_assemble, 
	[build_pdf/4, builtin_pdf_fonts/0, serialise2bin/1,
	 pdfloop/2]).

%% Set up Info, Catalog and Pages

init_pdf_context()->
    {{Year,Month,Day},{Hrs,Min,Sec}} = calendar:local_time(),
    #pdfContext{info=#info{creator="Erlang", 
			   creationDate= {{Year,Month,Day},{Hrs,Min,Sec}},
			   producer="erlguten-3.1", 
			   author="",
			   title="",
			   subject="",
			   keywords="ErlangKeyword"},
		images=dict:new(),
		fonts=[],
		currentpage=1,
		mediabox=pagesize(a4)}.

%% --------------------- User functions --------------

new()->
    spawn_link(fun() -> pdfloop(init_pdf_context(), []) end).

%% Export to PDF file format 

export(PID)->
    PID ! {export, self()},
    receive
	{export,PDFDoc}->
	    PDFDoc
    end.

%% clear up

delete(PID)->
    PID ! delete,
    done.


%% Add current page context to PDF document and start on a new page 
%% Note page 1 is already created  by default and  current page set 
%% to it after creation of PDF context.
new_page(PID)->
    PID ! {page, {new, self()}},
    receive
	{page,PageNo}->
	    PageNo
    end.

page_script(PID, Script) ->
    PID ! {page_script, Script}.

%% Go to a page already created.    
set_page(PID, PageNo)->
    PID ! {page,{set, PageNo}}.

%% Useful for page numbering functions etc.
get_page_no(PID)->
    PID ! {page, {get_no, self()}},
    receive
	{page,PageNo}->
	    PageNo
    end.
    
%% --- Info -----

set_author(PID,Author)->
    PID ! {info,{author, Author}}.

set_title(PID,Title)->
    PID ! {info,{title, Title}}.

set_subject(PID,Subject)->
    PID ! {info,{subject, Subject}}.

set_date(PID,Year,Month,Day)->
    PID ! {info,{date, {Year,Month,Day}}}.

set_keywords(PID, Keywords)->
    PID ! {info,{keywords, Keywords}}.


%% --- Page ---
pagesize(a0)             -> pagesize( 2380, 3368 );
pagesize(a1)             -> pagesize( 1684, 2380 );
pagesize(a2)             -> pagesize( 1190, 1684 );
pagesize(a3)             -> pagesize( 842, 1190 );
pagesize(a4)             -> pagesize( 595, 842 );
pagesize(a5)             -> pagesize( 421, 595 );
pagesize(a6)             -> pagesize( 297, 421 );
pagesize(a7)             -> pagesize( 210, 297 );
pagesize(a8)             -> pagesize( 148, 210 );
pagesize(a9)             -> pagesize( 105, 148 );
pagesize(b0)             -> pagesize( 2836, 4008 );
pagesize(b1)             -> pagesize( 2004, 2836 );
pagesize(b2)             -> pagesize( 1418, 2004 );
pagesize(b3)             -> pagesize( 1002, 1418 );
pagesize(b4)             -> pagesize( 709, 1002 );
pagesize(b5)             -> pagesize( 501, 709 );
pagesize(b6)             -> pagesize( 355, 501 );
pagesize(b7)             -> pagesize( 250, 355 );
pagesize(b8)             -> pagesize( 178, 250 );
pagesize(b9)             -> pagesize( 125, 178 );
pagesize(b10)            -> pagesize( 89, 125 );
pagesize(c5e)            -> pagesize( 462, 649 );
pagesize(comm10e)        -> pagesize( 298, 683 );
pagesize(dle)            -> pagesize( 312, 624 );
pagesize(executive)      -> pagesize( 542, 720 );
pagesize(folio)          -> pagesize( 595, 935 );
pagesize(ledger)         -> pagesize( 1224, 792 );
pagesize(legal)          -> pagesize( 612, 1008 );
pagesize(letter)         -> pagesize( 612, 792 );
pagesize(tabloid)        -> pagesize( 792, 1224 ).

pagesize(Width, Height) -> {0,0,Width,Height}.

set_pagesize(PID, Size)-> 
    PID ! {mediabox, pagesize(Size) }.

set_pagesize(PID, Width, Height) -> 
    PID ! {mediabox, pagesize(Width, Height)} .

%% -- Fonts --

set_font(PID, Fontname, Size)->
    PID ! {font, {set, Fontname, Size}}.

ensure_font_gets_loaded(PID, FontName) ->
    PID ! {ensure_font, FontName}.


%% -- This function is a bit expensive, but will stick to the public interface.
get_string_width(_PID, Fontname, PointSize, Str)->
    {richText, Inline} = eg_richText:str2richText(Fontname, PointSize, 0, default, true, Str),
    trunc(lists:foldl(fun(A, Accu) -> eg_richText:width(A) + Accu end, 0, Inline) /1000).

%% Text

begin_text(PID)-> append_stream(PID, pdf_op:begin_text() ).
    
end_text(PID)  -> append_stream(PID, pdf_op:end_text() ).
     
break_text(PID)-> append_stream(PID, pdf_op:break_text() ).
    
text(PID, Text) ->  append_stream_text(PID, pdf_op:text(Text) ).

textbr(PID,Text)-> append_stream_text(PID, pdf_op:textbr(Text) ).
    
kernedtext(PID, Text)-> append_stream_text(PID, pdf_op:kernedtext(Text) ).

set_text_pos(PID, X, Y)->  append_stream(PID, pdf_op:set_text_pos(X,Y)).
		      
set_text_leading(PID, L)-> append_stream( PID, pdf_op:set_text_leading(L) ).


set_text_rendering(PID, MODE) ->
    append_stream(PID, pdf_op:set_text_rendering(MODE) ).


set_char_space(PID, CS) -> append_stream(PID, pdf_op:set_char_space(CS) ).
    
set_word_space(PID, WS) -> append_stream(PID, pdf_op:set_word_space(WS) ).

set_text_scale(PID, SC) -> append_stream(PID, pdf_op:set_text_scale(SC) ).

set_text_rise(PID, RISE)-> append_stream(PID, pdf_op:set_text_rise(RISE)).


%% Graphics operators
path(PID, Type) -> append_stream(PID, pdf_op:path(Type)).
    
move_to(PID,P)-> append_stream(PID, pdf_op:move_to(P) ).

line(PID,From_To)          ->  append_stream(PID, pdf_op:line(From_To) ).
line(PID, From, To)        -> line(PID,{From, To}).
line(PID, X1, Y1, X2, Y2 ) -> line(PID, {{X1,Y1},{X2,Y2}}).

lines(PID, LineList)->  append_stream(PID, pdf_op:lines(LineList)).

%% Poly paths should be stroked/closed/filled with separate
%% command.

poly(PID,Points)->  append_stream(PID, pdf_op:poly(Points)).


%% Grid assumes sorted XLists and YList, minimum value first
grid(PID,XList,YList)->  append_stream(PID, pdf_op:grid(XList,YList)).

%% Bezier paths should be stroked/closed/filled with separate
%% command.

bezier(PID,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4})->
    append_stream(PID, pdf_op:bezier({X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4})).
bezier(PID,X1,Y1,X2,Y2,X3,Y3,X4,Y4)->
    bezier(PID,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}).

bezier_c(PID,Point1,Point2,Point3)->
    append_stream(PID, pdf_op:bezier_c(Point1,Point2,Point3)).

bezier_v(PID, Point1, Point2 )->
    append_stream(PID, pdf_op:bezier_v(Point1, Point2)).

bezier_y(PID, Point1, Point3)->
    append_stream(PID, pdf_op:bezier_y(Point1, Point3)).

arc(_PID,_X1,_Y1,_X2,_Y2)->
    tobedone.

circle(PID, {X,Y}, R)->
    append_stream(PID, pdf_op:circle( {X,Y}, R)).

ellipse(PID, {X, Y}, {RX, RY})->
    append_stream(PID, pdf_op:ellipse({X, Y}, {RX, RY})).


%% If Stroke Type is not appended in arguments, explicit
%% stroke command "path(StrokeType)" has to be executed.

rectangle(PID,{X,Y},{WX,WY}) ->
    rectangle(PID,X,Y,WX,WY).

rectangle(PID,{X,Y},{WX,WY}, StrokeType) ->
    rectangle(PID,X,Y,WX,WY,StrokeType).

rectangle(PID,X,Y,WX,WY) when is_pid(PID) ->
    append_stream(PID, pdf_op:rectangle(X,Y,WX,WY)).

rectangle(PID,X,Y,WX,WY,Option) ->
    append_stream(PID, pdf_op:rectangle(X,Y,WX,WY,Option)).

round_rect(PID, Point, Size, Radius)->
    append_stream(PID, pdf_op:round_rect(Point, Size, Radius)).


%% Line styles
set_line_width(PID,W)->
    append_stream(PID, pdf_op:set_line_width(W)).

set_line_cap(PID,Mode)->
    append_stream(PID, pdf_op:set_line_cap(Mode)).    

set_line_join(PID, Mode)->
    append_stream(PID, pdf_op:set_line_join(Mode)).    

set_miter_limit(PID,Limit)->
    append_stream(PID, pdf_op:set_miter_limit(Limit)).
        

set_dash(PID, Mode) -> 
    append_stream(PID, pdf_op:set_dash(Mode)).

set_dash(PID,Array,Phase)-> 
    append_stream(PID, pdf_op:set_dash(Array, Phase)).

%% Graphics state
save_state(PID)->
    append_stream(PID, pdf_op:save_state() ).
    
restore_state(PID)->
    append_stream(PID, pdf_op:restore_state() ).

%% Change geometry
transform(PID, A, B, C, D, E, F)->
    append_stream(PID, pdf_op:transform(A, B, C, D, E, F)).

translate(PID, X, Y)->
    append_stream(PID, pdf_op:translate(X,Y)).

scale(PID, ScaleX, ScaleY) when is_integer(ScaleX),is_integer(ScaleY)->
    append_stream(PID, pdf_op:scale(ScaleX, ScaleY)).

rotate(PID, Angle)->
    append_stream(PID, pdf_op:rotate(Angle)).


skew(PID, XScewAngle, YScewAngle)->
    append_stream( PID, pdf_op:skew(XScewAngle, YScewAngle) ).

mirror_yaxis(PID,Xtranslate)->
    append_stream(PID, pdf_op:mirror_yaxis(Xtranslate)).

mirror_xaxis(PID,Ytranslate)->
    append_stream(PID, pdf_op:mirror_xaxis(Ytranslate)).

%% Changing colors
%% Color value range 0 - 1
set_fill_color_CMYK(PID,C,M,Y,K)->
    append_stream(PID, pdf_op:set_fill_color_CMYK(C,M,Y,K)).

set_stroke_color_CMYK(PID,C,M,Y,K)->
    append_stream(PID, pdf_op:set_stroke_color_CMYK(C,M,Y,K)).

%% Color value range 0 - 1
set_fill_color_RGB(PID,R,G,B)->
    append_stream(PID, pdf_op:set_fill_color_RGB(R,G,B)).

set_stroke_color_RGB(PID,R,G,B)->
    append_stream(PID, pdf_op:set_stroke_color_RGB(R,G,B)).

%% Color is Name |{R,G,B}, Name = atom(), 0 < R,G,B < 255 
set_fill_color(PID, Color)->
    append_stream(PID, pdf_op:set_fill_color(Color)).

set_stroke_color(PID, Color)->
    append_stream(PID, pdf_op:set_stroke_color(Color)).

    
%% Gray 0.0-Black 1.0-White)
set_fill_gray(PID, Gray)->
    append_stream(PID, pdf_op:set_fill_gray(Gray) ).

set_stroke_gray(PID, Gray)->
    append_stream(PID, pdf_op:set_stroke_gray(Gray)).

%% Images
%% image(PID, FilePath )
%% image(PID, FilePath, Size)
%% image(PID, FilePath, Pos, Size)
%% Pos is {X,Y}
%% Size is {width, W} | {height, H} | {W,H}

image(PID, FilePath)->
    save_state(PID),
    image1(PID, FilePath, {size,{undefined,undefined}}),
    restore_state(PID).
    
image(PID, FilePath,Size)->
    save_state(PID),
    image1(PID, FilePath, Size),
    restore_state(PID).
    
image(PID, FilePath, {X,Y}, Size)  ->
    save_state(PID),
    translate(PID,X,Y),
    image1(PID, FilePath, Size),
    restore_state(PID).

image1(PID, FilePath, {max, undefined, H})->
    image1(PID, FilePath, {height, H});
image1(PID, FilePath, {max, W, undefined})->
    image1(PID, FilePath, {width, W});
image1(PID, FilePath, {max, W, H})->
    image1(PID, FilePath, {size, {max, W, H}});
image1(PID, FilePath, {width, W})->
    image1(PID, FilePath, {size,{W,undefined}});
image1(PID, FilePath, {height, H}) ->
    image1(PID, FilePath, {size,{undefined,H}});
image1(PID, FilePath, {W, H}) when is_integer(W), is_integer(H)->
    image1(PID, FilePath, {size,{W,H}});
image1(PID, FilePath, {size,Size})->
    PID ! {image, FilePath, Size}.



%% Internals
append_stream(PID, String)->
	PID ! {stream, {append, String}}.

append_stream_text(PID, String)->
	PID ! {stream, {append_text, String}}.


%% Turns the fontName into an index 1,2,3..
%% The first 14 are the standard fonts

fontName(Font) ->
    case eg_pdf_assemble:fontType(Font) of
	{_,Index} ->
	    Index;
	not_pdf ->
	    io:format("Font:~s is missing using Times-Roman~n", [Font]),
	    1
    end.

default_face() ->
    eg_richText:mk_face("Times-Roman", 12, true, default, 0).

allFonts() ->
    egFontMap:allFonts().

inBuiltFonts() ->
    ["Helvetica","Helvetica-Bold","Helvetica-Oblique","Helvetica-BoldOblique",
     "Times-Roman","Times-Bold","Times-Italic","Times-BoldItalic",
     "Courier","Courier-Bold","Courier-Oblique","Courier-BoldOblique",
     "Symbol", "ZapfDingbats"].

