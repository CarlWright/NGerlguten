%%======================================================================
%% erlguten_para_break.erl - break paragraphs
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

-module(erlguten_para_break).

%%-compile(export_all).

-export([break_para/4]).

-import(lists, [foreach/2, map/2, reverse/1, sort/2]).

-import(erlguten_geometry, [paraShape_measure/1, paraShape_next/1]).

%% Units 
%%       72 Points = 1"
%%       1  Pica = 12 Points  (1" = 6 picas = 72 points)

%% A4 = 11.69 x 8.27 inches
%%    = 842 x 595 points

%% A normal column is say 5" width = 30 picas

%% break a paragaph (in internal form) into lines
%% ParaShape = [Len1, Len2,...] = length of successive lines in the 
%%             paragraph when the list is exahaused the last value is repeated
%% FontMap   = maps XML tags onto {Break,FontNumber} pairs
%% Pts       = main point size of the text

break_para(Toks, PointSize, ParaShape, FontMap) ->
    Lines  = justify(Toks, ParaShape, PointSize),
    %% display_lines(Lines),
    Lines.

display_lines(Lines) ->
    foreach(fun(I) ->
		    io:format("~p~n",[erlguten_line_break:toks2str(I)])
	    end, Lines).

%% Units = Picas (1 Pica = 12 points, 72 Ppints = 1")

%% justify([Tok], [Widths], PointSize)
%%   Tok       = {wd2,FontNumber,Width,Str} | {sp2,FontIndex,Width}
%%   [Widths]  = lines widths per line in picas
%%   PointSize = int() = point size of font

%% How does this work
%%   We have a list which starts [{Cost, Para, LineWidths, Result}]
%%      For each iteration
%%      1) We split Para into {FirstLine, Rest} pairs in all reasonable ways
%%         where FirstLine fits into hd(lineWidths) with Cost Cost'
%%         From this we form {Cost'', Rest, tl(LineWidth), FirstLine ++ Result}
%%      2) We sort by Cost'' and retain the first N pairs
%%      3) When Para = [] we remove the list from the iterator and
%%         check the final value of Cost
%%         If this is better than the best value we keep it

justify(Para, ParaShape, Pts) ->
    {Cost, Lines} = iterate([{0,Para,ParaShape,[]}], none, 20, Pts),
    Lines.

iterate([], Best, Max, _) ->
    Best;
iterate(L, Best, Max, Pts) ->
    L1 = next_generation(L, Pts),
    L2 = sort(fun({I,_,_,_},{J,_,_,_}) -> I < J end, L1),
    L3 = trim(Max, L2),
    {Best1, L4} = finalise(L3, Best, []),
    iterate(L4, Best1, Max, Pts).

next_generation([], _)      -> [];
next_generation([H|T], Pts) -> 
    next(H, Pts) ++ next_generation(T, Pts). 

next({Cost, Para, ParaShape, Before}, Pts) ->    
    Measure     = paraShape_measure(ParaShape),
    ParaShape1  = paraShape_next(ParaShape),
    L = erlguten_line_break:split_para(Para, Measure, Pts),
    map(fun({Cost1, {Line, Para1}}) ->
		{Cost+Cost1, Para1, ParaShape1, [Line|Before]}
	end, L).

finalise([{Cost,[],_,Partition}|T], none, L) ->
    finalise(T, {Cost, reverse(Partition)}, L);
finalise([{Cost,[],_,Partition}|T], Best={C,_}, L) ->
    if 
	Cost < C ->
	    finalise(T, {Cost, reverse(Partition)}, L);
	true ->
	    finalise(T, Best, L)
    end;
finalise([H|T], Best, L) ->
    finalise(T, Best, [H|L]);
finalise([], Best, L) ->
    {Best, L}.

trim(0, L)     -> [];
trim(N, [H|T]) -> [H|trim(N-1, T)];
trim(N, [])    -> [].
    

    




