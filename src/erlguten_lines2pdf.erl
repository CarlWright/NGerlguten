%%======================================================================
%% erlguten_lines2pdf.erl - Convert internal form of line to PDF
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

-module(erlguten_lines2pdf).

%% There is a bug in this code: Blanks *inside*
%% strings are subject to expansion caused by the Tw operator
%% 

-export([lines2pdf/4]).

-import(lists, [map/2,reverse/1, reverse/2]).

-import(erlguten_geometry, [paraShape_measure/1, paraShape_next/1]).
-import(pdf, [i2s/1, f2s/1, flatten/1]).

%% line_to_pdf(X, Y, Measure, Toks) -> PDF.

%% This moves to X,Y and 

%% Toks = [tok()]
%% tok() = {wd2, FontNumber, Width, "String"} | {sp3,FontNumber,Width}]

%% Spaces within wd4 must never be expanded
%% spaces at sp *may* be expanded

%% Note sequences in a roman font will appear wd2,sp,wd2,sp,...
%% were there are no blanks in the wd2's
%% in a fixed width font (like courier) there may be spaced within the
%% words ...

%% FontNumber = The number of the font in the font server
%% Stretch    = Bool = true is spaces can be varied
%%            = false for fixed width fonts (like courier)
%% Width      = width + kerning of the string
%% Str        = character composing the string

%% Additional spaces are *only* inserted at the sp3 characters
%% *and* at the spaces inside the variable width fonts
%% Widths = [{Offset,Measure}] in picas

lines2pdf(Lines, PointSize, ParaShape, FontMap) ->
    CurrentFont = nofont, %% This will force a font change command
    lines2pdf(Lines, ParaShape, PointSize, FontMap, CurrentFont, []).

lines2pdf([H|T], ParaShape, Pts, FontMap, Font, L) ->
    LastLine      = length(T) == 0,
    Measure       = paraShape_measure(ParaShape),
    ParaShape1    = paraShape_next(ParaShape),
    {Font1, Code} = line2pdf(H, Measure, LastLine, Pts, FontMap, Font),
    lines2pdf(T, ParaShape1, Pts, FontMap, Font1, [Code|L]);
lines2pdf([], _, _, _, _, L) ->
    reverse(L).

%% generate Code for Toks
%% Toks = {wd2,Tag,Splittable,Len,Str}|{sp2,Tag,Width}

%% line2pdf(Toks, Measure, LastLine, PointSize, Font) ->
%%    {Font, Cmd}
%%    If LastList = true then left justify code
%%                = false then justify within Measure

line2pdf(Toks, Measure, true, PointSize, FontMap, Font) ->
    TotWidth = (erlguten_line_break:lineWidth(Toks) * PointSize)/1000,
    %% If this is the last line then we *might* have to do some
    %% micro justification. This occurs when the line width is greater
    %% Than the meansure - 
    case TotWidth > Measure*12 of
	true ->
	    line2pdf(Toks, Measure, false, PointSize, FontMap, Font);
	false ->
	    %% reset the Tw parameter to zero
	    Start = reverse("0 Tw "),
	    Wd3 = merge(Toks),
	    w3_2_pdf(Wd3, Font, PointSize, no, FontMap, Start)
    end;
line2pdf(Toks, Measure, LastLine, PointSize, FontMap, Font) ->
    %% io:format("Formatting line=Pts:~p ~p~n",
    %% [PointSize,erlguten_line_break:toks2str(Toks)]),
    %% First compute the correct gap
    TotWidth = (erlguten_line_break:lineWidth(Toks) * PointSize)/1000,
    %% io:format("Toks:~p~nerlguten_line_break:lineWidth(Toks)=~p TotWidth=~p~n"
    %% " measure (pts)=~p~n",
    %% [Toks, erlguten_line_break:lineWidth(Toks), TotWidth,
    %% Measure*12]),
    %% find the number of blanks
    NBlanks = length([X||{sp2,X,_}<-Toks]),
    Tw = if 
	     NBlanks > 0 ->
		 %% 12 pts = 1 pica
		 MeasureInPts = Measure*12,
		 Width = (MeasureInPts - TotWidth)/NBlanks,
		 %% io:format("Nblanks=~p Width=~p~n",[NBlanks, Width]),
		 {tw, Width};
	     NBlanks == 0 ->
		 no
	 end,
    %% io:format("Toks=~p~n",[Toks]),
    Wd3 = merge(Toks),
    %% io:format("Merged=~p~n",[Wd3]),
    Start = case Tw of
		no -> "";
		{tw, NN} -> reverse(tw(NN))
	    end,
    w3_2_pdf(Wd3, Font, PointSize, Tw, FontMap, Start).

%% mkPdf([wd3()], Font, Tw, L) -> {Font', Code}
%%   Font = the existing font

w3_2_pdf([{wd3,Font,Str}|T], Font, Pts, Tw, FontMap, L) ->
    %% Same font
    Pdf = str2pdf(Font, Str),
    w3_2_pdf(T, Font, Pts, Tw, FontMap, reverse(Pdf, L));
w3_2_pdf([{wd3, Font, Str}|T], _, Pts, Tw, FontMap, L) ->
    %% Different font
    Break = breakable(Font, FontMap),
    L1 = reverse(set_font(Font, Break, Tw, Pts), L),
    Pdf = str2pdf(Font, Str),
    w3_2_pdf(T, Font, Pts, Tw, FontMap, reverse(Pdf, L1));
w3_2_pdf([], Font, Pts, Tw, _, L) ->
    {Font, reverse(L)}.


str2pdf(Font, "")  -> "";
str2pdf(Font, Str) ->
    K = str2pdf1(Font, Str, [], []),
    K1 = map(fun({Str1,Kern}) -> {quote_strings(Str1), Kern} end, K),
    Pdf1 = ["[ ",map(fun({S,I}) ->
			      ["(", S,")",i2s(I)," "]
		      end, K1),
	    "] TJ\n"],
    flatten(Pdf1).
    
quote_strings([$(|T])  -> [$\\,$(|quote_strings(T)];
quote_strings([$)|T])  -> [$\\,$)|quote_strings(T)];
quote_strings([$\\|T]) -> [$\\,$\\|quote_strings(T)];
quote_strings([H|T])   -> [H|quote_strings(T)];
quote_strings([])      -> [].

%% We have to be careful here 
%% Certain characters must be quoted

str2pdf1(Font, [32,H|T], Tmp, L) ->
    str2pdf1(Font, [H|T], [32|Tmp], L);
str2pdf1(Font, [H,32|T], Tmp, L) ->
    str2pdf1(Font, T, [32,H|Tmp], L);
str2pdf1(Font, [H1,H2|T], Tmp,  L) ->
    case erlguten_font_server:kern(Font, {H1,H2}) of
	0 ->
	    str2pdf1(Font, [H2|T], [H1|Tmp], L); 
	N ->
	    Str = reverse([H1|Tmp]),
	    str2pdf1(Font, [H2|T], [], [{Str,-N}|L])
    end;
str2pdf1(Font, [H|T], Tmp, L) ->
    str2pdf1(Font, T, [H|Tmp], L);
str2pdf1(Font, [], [], L) ->
    reverse(L);
str2pdf1(Font, [], Tmp, L) ->
    reverse([{reverse(Tmp), 0}|L]).

set_font(Index, Bool, Tw, Size) ->
    %% io:format("setfont: Index:~p Bool=~p size:~p~n",
    %% [Index, Bool, Size]),
    "/F" ++ i2s(Index) ++ " " ++ i2s(Size) ++ " Tf\n" ++ set_tw(Bool, Tw).

set_tw(true, {tw, N}) -> tw(N);
set_tw(_, _)          -> tw(0).

tw(N) ->
    %% io:format("Here:TW=~p~n",[N]),
    [f2s(N), " Tw "].

%% To set the font use /Fn Pt Tf
%% [(A) 90 (W) 120 (A) 105 (Y again - correctly kerned) ] TJ

%%----------------------------------------------------------------------
%% Merge all tokens with the same font togther
%% merge([{wd2,Index,Bool,Str}|{sp2()]) -> [w3()]
%% wd2() = {wd2,Tag,Bool,Width,Str}

merge(Toks) -> 
    M = merge(Toks, []),
    %% io:format("Merged=~p~n",[M]),
    M.

merge([], L) ->
    reverse(L);
merge(Toks, L) ->
    {Val, Toks1} = collect_seq(Toks),
    merge(Toks1, [Val|L]).

collect_seq([{sp2,Tag,_}|T])       -> collect_seq(T, Tag, [$\s]);
collect_seq([{wd2,Tag,_,_,Str}|T]) -> collect_seq(T, Tag, reverse(Str)).

collect_seq([{sp2,Tag,_}|T], Tag, L) -> 
    collect_seq(T, Tag, [$\s|L]);
collect_seq([{wd2,Tag,_,_,Str}|T],Tag, L) -> 
    collect_seq(T, Tag, reverse(Str,L));
collect_seq(Toks, Tag, L) ->
    {{wd3,Tag,reverse(L)}, Toks}.

%%----------------------------------------------------------------------
%% misc

breakable(Index, [{_,Index,Bool, _}|_]) -> Bool;
breakable(Index, [_|T])                 -> breakable(Index, T);
breakable(Index, []) ->
    io:format("Invalid fontmap:~p~n", [Index]),
    exit(fatal).
