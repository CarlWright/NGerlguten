%% =====================================================================
%% hyphenation program - adapted from TeX
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

-module(erlguten_hyphenate).

-export([partitions/1, word/1, test/1]).
-import(lists, [reverse/1]).

%% -compile(export_all).

%% start-ed holly-wood
%% (science (wrong))

test(1) -> word("hyphenation");
test(2) -> partitions("supercalifragilisticexpialidocious").

partitions(W) -> partitions(word(W), [], []).

partitions([$-|T],  B, L) -> partitions(T, B, [{reverse(B), remove_hypthens(T)}|L]);
partitions([H|T], B, L)   -> partitions(T, [H|B], L);
partitions([], _, L)      -> reverse(L).

remove_hypthens([$-|T]) -> remove_hypthens(T);
remove_hypthens([H|T])  -> [H|remove_hypthens(T)];
remove_hypthens([])     -> [].

word(L) when length(L) > 4 ->
    case erlguten_hyphen_rules:exception(L) of
	no ->
	    L1 = word([$.|L] ++ ".", 0, []),
	    L2 = lists:sort(keep_odd(L1)),
	    %% io:format("L2=~p~n",[L2]),
	    W1 = make_word(L2, 1, L),
	    remove_singleton(W1);
	Str ->
	    Str
    end;
word(X) ->
    X.

%% -AAA... => AAA..
%% X-AAA.. => AAA..
%% ...-B   => ...B
%% ...B-   => ...B

remove_singleton([H,$-|T]) -> [H|remove_singleton1(T)];
remove_singleton([$-|T])   -> remove_singleton1(T);
remove_singleton(X)        -> remove_singleton1(X).

remove_singleton1([$-])   -> [];
remove_singleton1([$-,H]) -> [H];
remove_singleton1([H|T])  -> [H|remove_singleton1(T)];
remove_singleton1([])     -> [].

make_word([{Pos,C}|T], Pos, L) ->
    [$-|make_word(T, Pos, L)];
make_word(S=[{Pos,_}|_], Pos1, [H|T]) when Pos1 < Pos ->
    [H|make_word(S, Pos1+1, T)];
make_word([], _, L) ->
    L.

word([], _, L) ->
    L;
word(T, N, L) ->
    case erlguten_hyphen_rules:hyphens(T) of
	[] ->
	    word(tl(T), N+1, L);
	M ->
	    M1 = lists:map(fun({Pos,Val}) -> {Pos+N,Val} end, M),
	    %% io:format("~s ~p => ~p~n", [T, M, M1]),
	    L1 = merge(M1, L),
	    word(tl(T), N+1, L1)
    end.

merge([], L)          -> L;
merge([{Pos,C}|T], L) -> merge(T, merge1(Pos, C, L)).

merge1(Pos, C, [])                       -> [{Pos,C}];
merge1(Pos, C, [{Pos,C1}|T]) when C > C1 -> [{Pos,C}|T];
merge1(Pos, C, [{Pos,C1}|T])             -> [{Pos,C1}|T];
merge1(Pos, C, [H|T])                    -> [H|merge1(Pos, C, T)].

keep_odd(L) ->    
    lists:filter(fun({Pos,Count}) -> odd(Count) end, L).

odd(X) ->
    (X rem 2) == 1.

    
