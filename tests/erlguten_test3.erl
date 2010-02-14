%%======================================================================
%% erlguten_test3.erl - test cases
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
%% Last Edit: 2003-03-11
%% =====================================================================

-module(erlguten_test3).

%-compile(export_all).

-export([test/0, norm/0]).

-import(lists, [foldl/3, foreach/2, map/2, reverse/1, sort/2]).
-import(pdf, [i2s/1, f2s/1]).

-include("erlguten.hrl").

%% Units 
%%       72 Points = 1"
%%       1  Pica = 12 Points  (1" = 6 picas = 72 points)

%% A4 = 11.69 x 8.27 inches
%%    = 842 x 595 points

%% A normal column is say 5" width = 30 picas

%% x       = X coordinate of start of line in points
%% y       = Y coordinate of start of line in points
%% measure = Width of colum in picas 
%%           (a 5" column = 30 picas)
%% ptSize  = Typface size in points (72 points=1")
%% leading = Distance between two lines in points
%% nl      = Nl = number of lines in the bounding box for the text
%%
%% fontmap = [{Tag,Int,Bool,FontName}]
%%           Tag = code | em | raw => One of the tags that can
%%           occure in the XML (raw is implicit)
%%           Int = a sequence number 1 2 3 
%%           These will turn into F1 F2 F3 in the PDF
%%           FontName = name of font (these are standard names in Acrobat)

%% This maps the names of the XML tags onto names of fonts/line breaking
%% strategies

font_map0() ->
    [{raw,true,"Times-Roman"}, 
     {em,true,"Times-Italic"}, 
     {code,false,"Courier"}].

font_map1(PDF) ->
    L = font_map0(),
    map(fun({Tag,Bool,Font}) ->
			   Alias = pdf:get_font_alias(PDF, Font),
			   {Tag, Alias, Bool, Font}
		   end, L).

test() ->
    PDF = pdf:new(),
    F   = font_map1(PDF),
    %% io:format("Font Map1=~p~n",[F]),
    R1 = #paraBox{x=20,y=750,measure=17,ptSize=12,leading=14,nl=13,fontMap=F},
    XmlPara = parse_xml_para_str(xml(2)),
    PDF1 = demo(R1, XmlPara),
    R2 = #paraBox{x=250,y=760,measure=16,ptSize=8,leading=10,nl=9,fontMap=F},
    PDF2 = demo(R2,XmlPara),
    R3 = #paraBox{x=20,y=460,measure=40,ptSize=14,leading=16,nl=7,fontMap=F},
    PDF3 = demo(R3, XmlPara),
    Xml2 = parse_xml_para_str(xml(4)),
    R4 = #paraBox{x=400,y=600,measure=6,ptSize=12,leading=14,nl=3,fontMap=F},
    PDF4 = demo(R4,Xml2),
    Xml3 = parse_xml_para_str(xml(5)),
    R5 = #paraBox{x=20,y=800,measure=17,ptSize=12,leading=14,nl=2,fontMap=F},
    PDF5 = demo(R5,Xml3),
    Page1 = {page, [PDF1,PDF2,PDF3,PDF4, PDF5]},
    %Page1 = {page, [PDF1]},
    Info = #info{creator="Erlang", 
		 creationDate="20030215152011",
		 producer="mkPdf", 
		 author="",
		 title="",
		 subject="",
		 keywords="ErlangKeyword"}, 
    MediaBox={0,0,595,842},
    F2 = map(fun({Tag,Index,Bool,Font}) ->
		     {Font, "F" ++ integer_to_list(Index)}
	     end, F),
    erlguten_pdf_assemble:make_pdf_file("erlguten_test3.pdf", 
					Info, F2, [Page1], MediaBox).

parse_xml_para_str(Str) ->
    [{xml, XmlPara}] = erlguten_xml_lite:parse_all_forms(Str),
    XmlPara.

flatten(L) ->
    binary_to_list(list_to_binary(L)).

zip1([H1|T1],[H2|T2]) -> [[H1," ",H2]|zip1(T1, T2)];
zip1([], [])          -> [].

%% Typesetting a line is done in 5 phases (to make it easy :-)
%%
%% 1) Convert XML -> Internal form
%% 2) Linebreak the internal form using a set of line lengths
%% 3) Convert the lines to PDF strings
%% 4) Use the geomtry manager to position the lines
%% 5) zip the geometry commands with the PDF
 
demo(E, Xml) ->
    #paraBox{x=X,y=Y,measure=Measure,leading=Leading,nl=Nl,
	     fontMap=FontMap, ptSize=PointSize} = E,
    ParaShape = [Measure-1, Measure],
    %% io:format("FontMap=~p~n",[FontMap]),
    Toks  = erlguten_normalise_xml:normalise_xml(Xml, FontMap),
    Lines = erlguten_para_break:break_para(Toks, PointSize, ParaShape, 
					   FontMap),
    Pdf = erlguten_lines2pdf:lines2pdf(Lines, PointSize, ParaShape, FontMap),
    %% io:format("PDF=~p~n",[map(fun(I) -> flatten(I) end, Pdf)]),
    Posns = erlguten_geometry:mk_line_headers(X, Y-Leading, Measure, Leading, 
					 ParaShape, length(Pdf)),
    %% io:format("Posns=~p~n",[map(fun(I) -> flatten(I) end, Posns)]), 
    Pdf1 = ["BT\n", zip1(Posns, Pdf), "ET\n"],
    Pdf2 = flatten(Pdf1),
    [erlguten_geometry:draw_box(X, Y, Measure, Leading, Nl), Pdf2].

%%----------------------------------------------------------------------
%% test data sets

%% Here are some widths
%% Times Roman A = 722
%% space = 250
%% a = 444
%% b = 500
%% c = 444
%% W = 944

xml(1) ->
    "<p>aaa aaa aaa aaa bbb ccc ddd eee aaa ddd ss aaa aaa aaa 
        bbb bbb bbb bbb bbb bbb bbb ccc ddd
     </p>";
xml(3) ->
    "<p>This is normal text, with no emphasised code, 
the next example will be more complicated. This example
is just simple text. In the next example I will show some
text with emphasis.</p>";
xml(2) ->
    "<p>This is normal text, set 5 picas wide in 12/14 Times Roman.
I even allow some <em>emphasised term,</em> set in Times-Italic. The TeX
hyphenation algorithm is also implemented.
I have also some <em>cursive text</em> and an example of
an Erlang term. The term <code>{person, \"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY again is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>";
xml(4) ->
    "<p>This is Times Roman.</p>";
xml(5) ->
    "<p>is correctly kerned! Erlang terms <code>{like, this}</code>are typeset in <em>courier.</em></p>".

norm() -> "(This is normal text, with some **emphasised code**, I have
    also some *cursive text* and an example of and Erlang term. The
    term <{person, \"Joe\"}> is an Erlang term.  The variable <X>, was
    immediately followed by a comma.)".











