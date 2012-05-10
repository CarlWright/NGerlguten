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

-module(eg11_test).
-include_lib("eunit/include/eunit.hrl").
-define(IMAGE_DIR, "../test/images/").
%% ============================================================================

run_test()->
    ?debugMsg("Test Begin"),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_page(PDF,1),
    eg_pdf:set_font(PDF, "Victorias-Secret", 14),    
    eg_pdf_lib:moveAndShow(PDF, 350, 185, "Type 4"),
    
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,red),
    eg_pdf:rectangle(PDF, 350,200,16,16, fill),
    eg_pdf:restore_state(PDF),
        
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,red),
    eg_pdf:rectangle(PDF, 340,480,56,56, fill),
    eg_pdf:image(PDF,?IMAGE_DIR ++ "page_white_text.png", {350,500},{height,16}),
    eg_pdf:restore_state(PDF),
    
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("./eg_test11.pdf",[Serialised]),
    eg_pdf:delete(PDF).

