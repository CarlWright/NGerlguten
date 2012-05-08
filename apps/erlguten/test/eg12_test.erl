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

-module(eg12_test).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================

run_test()->
    ?debugMsg("Begin Test"),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_page(PDF,1),
    
    eg_pdf:set_font(PDF, "Victorias-Secret", 14),  
    eg_pdf_lib:moveAndShow(PDF, 50, 20, "Type 6"),

    
    eg_pdf:save_state(PDF),
    eg_pdf:set_fill_color(PDF,gainsboro),
    eg_pdf:rectangle(PDF, 50,35,240,320, fill),
    io:format("Patience, this takes a bit.",[]),
    case eg_pdf:image(PDF,"../testing/images/dice.png",{50,35}, {240,320}) of
	{error, Reason} -> 
	    io:format("Image processing error with file ~s~n",[Reason]);
	ok -> 
	    ok
    end,

    eg_pdf:restore_state(PDF),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("./eg_test12.pdf",[Serialised]),
    eg_pdf:delete(PDF).

