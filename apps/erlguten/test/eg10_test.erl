%%==========================================================================
%% Copyright (C) 2010 Carl Wright
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
%% Purpose: PNG image testing
%%==========================================================================

-module(eg10_test).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================

run_test()->
    ?debugMsg("Begin Test"),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_page(PDF,1),

    io:format("Patience, this takes a bit.",[]),    
    eg_pdf:set_font(PDF, "Victorias-Secret", 14),
    eg_pdf_lib:moveAndShow(PDF, 100, 685, "Type 0"),
    eg_pdf:image(PDF,"../testing/images/web-links.png", {100,700},{height,21}),
    
    eg_pdf_lib:moveAndShow(PDF, 150, 685, "Type 2"),
    eg_pdf:image(PDF,"../testing/images/ruport.png", {150,700},{width,258}),
    
    eg_pdf_lib:moveAndShow(PDF, 100, 485, "Type 3"),
    eg_pdf:image(PDF,"../testing/images/rails.png", {100,500},{height,50}),
    
    eg_pdf_lib:moveAndShow(PDF, 350, 185, "Type 4"),
    eg_pdf:image(PDF,"../testing/images/page_white_text.png", {350,500},{height,16}),
    
    eg_pdf_lib:moveAndShow(PDF, 50, 20, "Type 6"),
    eg_pdf:image(PDF,"../testing/images/dice.png",{50,35}, {height,320}),
    
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("./eg_test10.pdf",[Serialised]),
    eg_pdf:delete(PDF).

