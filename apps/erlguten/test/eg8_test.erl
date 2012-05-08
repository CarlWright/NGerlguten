%%==========================================================================
%% Copyright (C)  2010 Carl Wright
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
%% Authors:   Carl Wright <wright@servicelevel.net>
%% Purpose: Test eg_table module.
%%==========================================================================

-module(eg8_test).
-include("eg.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================

run_test_()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,letter),
    eg_pdf:set_page(PDF,1),

    Fonts = #table{},
    
    Rows = [{row,[],
        [{cell,[],[{b,[], [{raw, "Version"}]}]},
         {cell,[],[{b,[], [{raw, "Status"}]}]}]}
        ,
        {row,[],
          [{cell,[],[{raw, "0.1"}]},
           {cell,[],[{raw, "Ready"}]}]}],
           
           
    
   _Var = eg_table:table(PDF, Rows, 50, 450,700,50,10, Fonts),

    A = "<row><cell>Heading 1</cell><cell>Heading 2</cell><cell>Heading 3</cell></row>
      <row><cell>Content 1</cell><cell>Content 2</cell><cell>Content 3</cell></row>",
    B = eg_xml_lite:parse_all_forms(A),   % parse the xml stream
    [{xml,C},{xml,D}] = B,                % remove the stuff that "table" doesn't want
    _Var2 = eg_table:table(PDF, [C,D], 120,300,500,50,10, Fonts),
    
    

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

    _Var3 = eg_table:table_from_xml(PDF, A1, 120,400,350,50,10, Fonts),
    
    Smaller = "<row><cell>Escape Sequence</cell><cell>Value</cell></row>
      <row><cell>\\b</cell><cell>Backspace</cell></row>
      <row><cell>\\d</cell><cell>Delete</cell></row>
      <row><cell>\\e</cell><cell>Escape</cell></row>",  
      
    _Var4 = eg_table:table_from_xml(PDF, Smaller, 350,150,700,50,10, Fonts),
    _Var5 = eg_table:table_from_xml(PDF, Smaller, 350,250,550,50,14, Fonts),
    _Var6 = eg_table:table_from_xml(PDF, Smaller, 350,300,300,50,18, Fonts), 

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("./eg_test8.pdf",[Serialised]),
    eg_pdf:delete(PDF),
    test_done.

