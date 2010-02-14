%%======================================================================
%% erlguten_normalise_xml.erl
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


-module(erlguten_normalise_xml).

-export([normalise_xml/2]).

-import(lists, [foldl/3, foreach/2, map/2, reverse/1, reverse/2, sort/2]).

-import(pdf, [i2s/1, f2s/1]).

-include("erlguten.hrl").

%%----------------------------------------------------------------------
%% Normalise the XML
%%   Input = XML parse tree
%%   OutPut = {sp1, Tag} | {wd1, Tag, Str}
%%   If the font is splitable the input is split into sequences
%%   at the space character.
%%   Of words and spaces. Non-splittable fonts like
%%   Courier are kept together
 
normalise_xml(X, FontMap) ->
    Toks = reverse(normalise(X, FontMap, [])),
    %% io:format("Here Toks=~p~n",[Toks]),
    map(fun(I) -> width(I, FontMap) end, Toks).

normalise({_, _, X}, FontMap, L) ->
    normalise_para(X, FontMap, L);
normalise(Z, _, L) ->
    io:format("I cannot normalise:~p~n",[Z]).

normalise_para(Items, FontMap, L) ->
    foldl(fun(I, L0) ->
		  normalise_para_item(I, FontMap, L0)
	  end, L, Items).
    
normalise_para_item({raw,Str}, FontMap, L) ->
    case breakable(raw, FontMap) of
	true ->
	    normalise_str(Str, raw, L);
	false ->
	    [{wd1,raw,false,Str}|L]
    end;
normalise_para_item({Tag, _, [{raw,Str}]}, FontMap, L) ->
    case breakable(Tag, FontMap) of
	true ->
	    normalise_str(Str, Tag, L);
	false ->
	    [{wd1,code,false,Str}|L]
    end.


breakable(Tag, [{Tag,_,Bool, _}|_]) -> Bool;
breakable(Tag, [_|T])               -> breakable(Tag, T);
breakable(Tag, []) ->
    io:format("Invalid fontmap:~p~n", [Tag]),
    exit(fatal).

normalise_str([H|T], Tag, L) ->
    case is_white(H) of 
	true ->
	    T1 = skip_white(T),
	    normalise_str(T1, Tag, [{sp1,Tag}|L]);
	false ->
	    {Word, T1} = collect_word(T, [H]),
	    normalise_str(T1, Tag, [{wd1,Tag,true,Word}|L])
    end;
normalise_str([], _, L) ->
    L.

%% End Normalise XML
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% misc

is_white($\s) -> true;
is_white($\n) -> true;
is_white($\r) -> true;
is_white($\t) -> true;
is_white(_)   -> false.

skip_white(X=[H|T]) ->
    case is_white(H) of
	true  -> skip_white(T);
	false -> X
    end;
skip_white([]) ->
    [].

collect_word(X=[H|T], L) ->
    case is_white(H) of
	true  -> {reverse(L), X};
	false -> collect_word(T, [H|L])
    end;
collect_word([], L) ->
    {reverse(L), []}.

width({wd1, Tag, Bool, Str}, FontMap) ->
    Index = indexOf(Tag, FontMap),
    {wd2, Index, Bool, erlguten_line_break:sizeof(Index, Str), Str};
width({sp1, Tag}, FontMap) ->
    Index = indexOf(Tag, FontMap),
    {sp2, Index, erlguten_font_server:char_width(Index, $\s)}.

%% Units = Picas (1 Pica = 12 points, 72 Ppints = 1")


indexOf(Tag, [{Tag,Index,_,_}|_]) ->
    Index;
indexOf(Tag, [_|T]) ->
    indexOf(Tag, T);
indexOf(Tag, []) ->
    io:format("Invalid fontmap:~p~n", [Tag]),
    exit(fatal).

