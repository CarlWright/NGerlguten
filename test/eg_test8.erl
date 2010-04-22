%%==========================================================================
%% Copyright (C) 2003 Joe Armstrong
%%              2010 Carl Wright
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
%% Authors:   Joe Armstrong <joe@sics.se>
%% Purpose: Grid planning sheet
%%==========================================================================

-module(eg_test8).

-export([test/0]).

%% ============================================================================

test()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,letter),
    eg_pdf:set_page(PDF,1),
    
    Rows = [{row,[],
        [{cell,[],[{b,[], [{raw, "Version"}]}]},
         {cell,[],[{b,[], [{raw, "Status"}]}]}]}
        ,
        {row,[],
          [{cell,[],[{raw, "0.1"}]},
           {cell,[],[{raw, "Ready"}]}]}],
    
   Var = eg_table:table(PDF, Rows, 50, 450,700,50,10),

    A = "<row><cell>Heading 1</cell><cell>Heading 2</cell><cell>Heading 3</cell></row>
      <row><cell>Content 1</cell><cell>Content 2</cell><cell>Content 3</cell></row>",
    B = eg_xml_lite:parse_all_forms(A),
    [{xml,C},{xml,D}] = B,
    Var2 = eg_table:table(PDF, [C,D], 120,300,500,50,10),

    A1 = "<row><cell>Escape Sequence</cell><cell>Value</cell></row>
      <row><cell>\\b</cell><cell>Backspace</cell></row>
      <row><cell>\\d</cell><cell>Delete</cell></row>
      <row><cell>\\e</cell><cell>Escape</cell></row>
      <row><cell>\\f</cell><cell>Form Feed</cell></row>
      <row><cell>\\n</cell><cell>New line</cell></row>
      <row><cell>\\r</cell><cell>Carriage Return</cell></row>
      <row><cell>\\s</cell><cell>Space</cell></row>
      <row><cell>\\t</cell><cell>Tab</cell></row>
      <row><cell>\\v</cell><cell>Vertical Tab</cell></row>
      <row><cell>\\NNN \\NN \\N</cell><cell>Octal characters (N is 0..7)</cell></row>
      <row><cell>\\^a..\\^z or \\^A..\\^Z</cell><cell>Ctrl+A to Ctrl+Z</cell></row>
      <row><cell>\\'</cell><cell>Single quote</cell></row>",

    Var3 = eg_table:table_from_xml(PDF, A1, 120,400,350,50,10),


    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("../test/eg_test8.pdf",[Serialised]),
    eg_pdf:delete(PDF).

