%%==========================================================================
%% Copyright (C) 2004 Sean Hinde
%%               2010 Carl Wright
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% Authors: Sean Hinde <sean.hinde@mac.com>, Carl Wright <wright@servicelevel.net>
%% Purpose: Add API call to create a table in a document.
%%==========================================================================

-module (eg_block).

-export([block/11, block/10,  colored_inner_block/11, inner_block/10]).
    
-include("../include/eg.hrl").


%% 
%% @doc block() processes one or more paragraphs correctly into PDF content
%%
%%  parameters to control its output
%%
%% PDF  = the PID of pdf process for the document
%%
%% Color  = the color of the box that the block of text is in
%%
%%  Sample  = the content to format as a block of text i.e. "<p> a para with <em> big </em> thoughts </p>"
%%
%%  X = the X coordinate of the top left corner of the block of text
%%
%%  Y = the Y coordinate of the top left corner of the block of text 
%%
%%  Measure = the width of the block holding the text in points
%%
%%  PtSize  = the size in points of the font
%%
%%  Leading = the distance in points from the bottom of one line to the bottom of the next 
%%
%%  NLines  = the number of lines allowed in the block of text
%%
%%  Justification = an atom expressing the text justification required ()
%%
%%  TagMap  = the tagmap describing how to handle differnet XML tags
%%
%% the tag map is formatted like the following:
%%
%% <pre>
%% default_tagMap(Pts) -> 
%%     {[p],
%%      [{default,eg_richText:mk_face("Times-Roman", Pts, true, default, 0)}, 
%%       {em,     eg_richText:mk_face("Times-Italic", Pts, true, default, 0)}, 
%%       {red,    eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true,  {1,0,0},0)},  
%%       {blue,   eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true, {0,0,1},0)},  
%%       {code,   eg_richText:mk_face("Courier", Pts, false, default, 0)},  
%%       {b,      eg_richText:mk_face("Times-Bold", Pts, true, default, 0)}, 
%%       {hb,     eg_richText:mk_face("Helvetica-Bold", Pts, true, default, 0)},  
%%       {helv,   eg_richText:mk_face("Helvetica", Pts, true, default, 0)}  
%%      ]}.
%% </pre>


%% process an XML block of content into a block of PDf text with a color background

block(PDF, Color, Sample, X, Y, Measure, PtSize, Leading, NLines, Justification, TagMap) ->
    Width = Measure + 20,
    Ht = (NLines * Leading) + 20,
    box(PDF, Color, X, Y-Ht+10, Width, Ht),
    block(PDF, Sample, X+10, Y , Measure, PtSize, Leading, NLines, Justification, TagMap).

%% @doc process a parsed XML block of content into a block of PDf text with a color background 
   
colored_inner_block(PDF, Color, Sample, X, Y, Measure, PtSize, Leading, NLines, Justification, TagMap) ->
    Width = Measure + 20,
    Ht = (NLines * Leading) + 20,
    box(PDF, Color, X, Y-Ht+10, Width, Ht),
    inner_block(PDF, Sample, X+10, Y-10, Measure, PtSize, Leading, NLines, Justification, TagMap).

%% @doc process an XML block of content into a block of PDf text with a blank background
       
block(PDF, Sample, X, Y, Measure, PtSize, Leading, NLines, Justification, TagMap) ->
    inner_block(PDF, eg_xml_lite:parse_all_forms(Sample),X, Y, Measure, PtSize, Leading, NLines, Justification, TagMap).

%% @doc process a parsed XML block of content into a block of PDf text with a blank background

inner_block(PDF, [{raw, Xml}], X, Y, Len, PtSize, Leading, NLines, Justification, TagMap) ->
   block2(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, NLines, Justification, TagMap),
   ok;
inner_block(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, NLines, Justification, TagMap) ->
   block2(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, NLines, Justification, TagMap),
   ok;
inner_block(PDF, [{xml, Xml} | T], X, Y, Len, PtSize, Leading, NLines, Justification, TagMap) ->
  Height = block2(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, NLines, Justification, TagMap),
   inner_block(PDF, T, X, Y - Height, Len, PtSize, Leading, NLines, Justification, TagMap).

    
block2(PDF, [{xml, Xml}], X, Y, Len, _PtSize, Leading, NLines, Justification, TagMap) ->
    ensure_fonts_are_loaded(PDF, TagMap),
    Norm = eg_xml2richText:normalise_xml(Xml, TagMap),
    %% io:format("Norm=~p~n",[Norm]),
    {p, _, RichText} = Norm,
    Widths = [Len-20|lists:duplicate(NLines-1, Len)],
    Off = [20|lists:duplicate(NLines-1, 0)],
    case eg_line_break:break_richText(RichText, { Justification, Widths}) of
	impossible ->
	    io:format("Cannot break line are widths ok~n");
	{Lines,_,_} ->
	    Code = eg_richText2pdf:richText2pdf(PDF, X, Y, Justification, 0, Lines, 
						Leading, Widths, Off),
	    eg_pdf:begin_text(PDF),
	    eg_pdf:append_stream(PDF, Code),
	    eg_pdf:end_text(PDF),
	    length(Lines) * Leading

    end.  
    

box(PDF, Color, X, Y, W, H) ->
    eg_pdf:set_fill_color(PDF, Color), 
    eg_pdf:rectangle(PDF,{X, Y},{W,H}),
    eg_pdf:path(PDF,fill),
    eg_pdf:set_fill_color(PDF,black).
    

ensure_fonts_are_loaded(PDF, {_,TagMap}) ->
    lists:foreach(fun({_,Face}) ->
			  FontHandler = eg_richText:fontFromFace(Face),
			  Font = FontHandler:fontName(),
			  eg_pdf:ensure_font_gets_loaded(PDF, Font)
		  end, TagMap).