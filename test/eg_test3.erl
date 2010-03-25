%%======================================================================
%% eg_test3.erl - test cases
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

-module(eg_test3).

-compile(export_all).

-export([test/0, norm/0]).
    
-import(eg_line_break, [break_richText/2]).
-import(eg_richText2pdf, [richText2pdf/8]).

-import(lists, [foldl/3, foreach/2, map/2, reverse/1, sort/2]).
-import(pdf_op, [i2s/1, f2s/1]).

-include("../src/eg.hrl").

box(PDF, Color, X, Y, W, H) ->
    pdf:set_fill_color(PDF, Color), 
    pdf:rectangle(PDF,{X, Y},{W,H}),
    pdf:path(PDF,fill),
    pdf:set_fill_color(PDF,black).
    

test() ->
    PDF = pdf:new(),    
    pdf:set_pagesize(PDF,a4),
    pdf:set_page(PDF,1),
    pdf_lib:showGrid(PDF, a4),
    box(PDF, moccasin, 50, 300, 225, 350), 
    zap(PDF, gold, title, 20, 830, 66, 24, 24, 1),
    zap(PDF, simple, 60, 650, 35, 14, 16, 5),
    zap(PDF, simple, 60, 560, 30, 14, 16, 5),
    zap(PDF, whitesmoke, two, 300, 760, 44, 18,20, 3),
    zap(PDF, romanAndCourier1, 60, 360, 35, 14, 16, 7),
    zap(PDF, palegreen, complex, 400, 600, 26, 12, 14, 22),
    zap(PDF, 5, 60, 450, 35, 12, 14, 6),
    zap(PDF, azure, narrow, 280, 650, 16, 8,10, 38),
    %% pdf_lib:showGrid(PDF, a4),
    pdf:image(PDF,'../test/joenew.jpg',{50, 650},{width,200}),
    Serialised = pdf:export(PDF),
    file:write_file("../test/eg_test3.pdf",[Serialised]),
    pdf:delete(PDF).

ensure_fonts_are_loaded(PDF, {_,TagMap}) ->
    foreach(fun({_,Face}) ->
		    FontHandler = eg_richText:fontFromFace(Face),
		    Font = FontHandler:fontName(),
		    pdf:ensure_font_gets_loaded(PDF, Font)
	    end, TagMap).


zap(PDF, Color, Sample, X, Y, Measure, PtSize, Leading, NLines) ->
    Width = Measure*6 + 20,
    Ht = NLines * PtSize + 20,
    box(PDF, Color, X, Y-Ht+10, Width, Ht),
    zap(PDF, Sample, X+10, Y+10, Measure, PtSize, Leading, NLines).
    
zap(PDF, Sample, X, Y, Measure, PtSize, Leading, NLines) ->
    %% Measure in picas 
    Len = Measure*6,
    Xml = parse_xml_para_str(xml(Sample)),
    %% io:format("XML=~p~n",[Xml]),
    TagMap = eg_xml2richText:default_tagMap(PtSize),
    ensure_fonts_are_loaded(PDF, TagMap),
    Norm = eg_xml2richText:normalise_xml(Xml, TagMap),
    %% io:format("Norm=~p~n",[Norm]),
    {p, _, RichText} = Norm,
    Widths = [Len-20|lists:duplicate(NLines-1, Len)],
    Off = [20|lists:duplicate(NLines-1, 0)],
    case break_richText(RichText, {justified, Widths}) of
	impossible ->
	    io:format("Cannot break line are widths ok~n");
	{Lines,_,_} ->
	    Code = richText2pdf(X, Y, justified, 0, Lines, 
				Leading, Widths, Off),
	    pdf:begin_text(PDF),
	    pdf:append_stream(PDF, Code),
	    pdf:end_text(PDF)
%%	    pdf_lib:draw_box(PDF, X, Y, Len, Leading, NLines)
    end.

parse_xml_para_str(Str) ->
    [{xml, XmlPara}] = eg_xml_lite:parse_all_forms(Str),
    XmlPara.

flatten(L) ->
    binary_to_list(list_to_binary(L)).

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
xml(narrow) ->
    "<p>This is a long narrow box set in Times-Roman. Times-Roman was
designed for printing long and narrow newspaper columns. It actually looks    
pretty horrid if set in wide measures. This is set narrow and tight. The 
really catestrophic thing about Times-Roman is that is is probably the
most commonly used typeface, despite the fact it in manifestly
unsuitable for the purpose it is being used for. Using narrow columns
you can cram in loads of virtually unreadable data - no body will thank
you, apart from environmentalists, who, I suppose will be pleased at the
number of trees which are being saved.</p>";
xml(simple) ->
    "<p>This is normal text, with no emphasised code, 
the next example will be more complicated. This example
is just simple text. In the next example I will show some
text with emphasis.</p>";
xml(two) ->
    "<p>This is normal text, with a small
ammount of <em>emphasised</em> text.
This example only has two typefaces.</p>";
xml(romanAndCourier1) ->
    "<p>This is normal text, with a small
ammount of <code>courier</code> text.
This example only has two typefaces.</p>";
xml(complex) ->
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
    "<p>is <red>AWAY</red> correctly kerned? Erlang terms <code>{like, this}</code>are typeset in <em>courier.</em> The <red>red terms are typeset in
ZapfChancery-MediumItalic.</red> Then I can set <blue>blue</blue> terms as
well.</p>";
xml(title) ->
    "<p>This page tests justification routines</p>".

norm() -> "(This is normal text, with some **emphasised code**, I have
    also some *cursive text* and an example of and Erlang term. The
    term <{person, \"Joe\"}> is an Erlang term.  The variable <X>, was
    immediately followed by a comma.)".


