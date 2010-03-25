%%======================================================================
%% Line breaking algorithm
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
%% Last Edit: 2003-03-19
%% =====================================================================

-module(eg_line_break).

%% To do 
%% Make sure we exit the split para recursion if the paragraph is to
%% short

%% para_break ONLY computes the break points

-export([break_richText/2]).


-export([make_partitions/1]). % for testing only
                              % this export can be removed later

-import(lists, [foldl/3, foreach/2, reverse/1, reverse/2,
		sort/2, map/2, filter/2]).

-import(eg_richText,  [cloan_space/1,
		       width/1,
		       is_space/1, is_nl/1, is_word/1,
		       is_breakable/1, lineWidth/1,
		       richText2str/1,
		       string/1, cloan_word/2,
		       widthExcludingSpaces/1]).

%% -define(DEBUG, true).

-ifdef(DEBUG).
dbg_io(Str) -> dbg_io(Str,[]).
dbg_io(Str,Args) ->
    io:format("eg_line_break: ~p " ++ Str, [self()] ++ Args),
    ok.
-else.
dbg_io(_) -> ok.
dbg_io(_,_) -> ok.
-endif.

%% breakRichText(richText, Type, Widths) ->
%%    {Lines,Widths', Spill}

%% Type = justified | ragged | preformatted | centered
%% Widths = [Int] = lengths of the lines

%% justified    = Optimally break each line depending upon the widths
%%                doing hyphenation where possible and required
%% ragged       = break on first fit without hyphenation
%% preformatted = break on NL
%% centered     = breal on NL

break_richText({richText,T},{justified, W}) -> 
    text2para_widths(T,justified,W);
break_richText({richText,T},{ragged,W}) -> 
    text2para_widths(T, ragged, W);
break_richText({richText,T},{preformatted,W}) -> 
    break_on_nl(T, W, []);
break_richText({richText,T},{centered,W}) -> 
    break_on_nl(T, W, []).

text2para_widths(Txt, ParaShape, Widths) ->
    Txt1 = replace_nls_with_spaces(Txt),
    %% debug("text2", Txt1),
    case ParaShape of
	ragged    -> text2para_ragged(Txt1, Widths, []);
	justified -> justify(Txt1, Widths)
    end.

replace_nls_with_spaces(L) ->
    map(fun(I) ->
		case is_nl(I) of
		    true  -> cloan_space(I);
		    false -> I
		end
	end, L).

text2para_ragged(Toks, [], L) ->
    {finalise_ragged(L), [], Toks};
text2para_ragged([], Widths, L) ->
    {finalise_ragged(L), Widths, []};
text2para_ragged(Lines, [W], L) ->
    {Line, Rest} = first_break_line(Lines, W),
    text2para_ragged(Rest, [W], [Line|L]);
text2para_ragged(Lines, [H|T], L) ->
    {Line, Rest} = first_break_line(Lines, H),
    text2para_ragged(Rest, T, [Line|L]).

finalise_ragged(Final) ->
    map(fun({_Cost,_Len,Line}) -> {richText, Line} end, reverse(Final)).

first_break_line(Toks, Len) ->
    Toks1 = remove_leading_spaces(Toks),
    first_break_line(Toks1, 0, Len*1000, []).

first_break_line(All=[H|T], Sum, Len, L) ->
    Sum1 = Sum + width(H),
    case Sum1 < Len of
	true -> 
	    first_break_line(T, Sum1, Len, [H|L]);
	false when L == [] ->
	    {{Sum1, Len, remove_leading_spaces([H])},
	     remove_leading_spaces(T)};
	false ->
	    {{Sum,Len, reverse(remove_leading_spaces(L))}, 
	     remove_leading_spaces(All)}
    end;
first_break_line([], Sum, Len, L) ->
    {{Sum,Len,reverse(remove_leading_spaces(L))}, []}.

remove_leading_spaces(X=[H|T]) ->
    case is_space(H) of
	true  -> remove_leading_spaces(T);
	false -> X
    end;
remove_leading_spaces([]) -> [].

%% break_on_nl

break_on_nl(Toks, [], L)     -> {reverse(L), [], {richText, Toks}};
break_on_nl([], T, L)        -> {reverse(L), T, []};
break_on_nl(Toks, [H|T], L)  ->
    T1 = if T == [] -> [H];
            true    -> T
         end,
    {Line, Toks1} = collect_line(Toks, []),
    break_on_nl(Toks1, T1, [{richText, Line}|L]).

collect_line([H|T], L) ->
    case is_nl(H) of
	true  -> {reverse(L), T};
	false -> collect_line(T, [H|L])
    end;
collect_line([], L) ->
    {reverse(L), []}.

%%------------------------------------------------------------------------

display_lines(Lines) ->
    foreach(fun(I) ->
		    dbg_io("~s~n",[r2s(I)])
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

%% Iternate [{Cost,Text,Widths,Broken}]

justify(Text, Widths) ->
    %% dbg_io("justify Text=~p widths=~p~n",[Text,Widths]),
    case iterate([{0,Text,Widths,[]}], none, 20) of
	{Cost, Lines, Widths1, Spill} ->
	    Lines1 = map(fun(I) -> {richText, I} end, Lines),
	    {Lines1, Widths1, {richText, Spill}};
	none ->
	    impossible
    end.

iterate([], Best, Max) ->
    Best;
iterate(L, Best, Max) ->
    L1 = next_generation(L),
    L2 = sort(fun({I,_,_,_},{J,_,_,_}) -> I < J end, L1),
    L3 = trim(Max, L2),
    {Best1, L4} = finalise(L3, Best, []),
    iterate(L4, Best1, Max).

next_generation([])    -> [];
next_generation([H|T]) -> next(H) ++ next_generation(T).

%% next({Cost, RichText, Widths*, Before})

next({Cost, Text, [H|T], Before}) ->    
    L = break_line(Text, H),
    T1 = if T == [] ->
                 [H];
            true ->
                 T
         end,
    map(fun({Cost1, {Line, Text1}}) ->
		{Cost+Cost1*Cost1, Text1, T1, [Line|Before]}
	end, L).

%% finalising an item means we move it out of the iterator
%% into the final value

%% the first two cases are when we terminate normally
finalise([{Cost,More,[],Partition}|T], none, L) ->
    finalise(T, {Cost, reverse(Partition), [], More}, L);
finalise([{Cost,More,[],Partition}|T], Best={C,_,_,_}, L) ->
    if 
	Cost < C ->
	    finalise(T, {Cost, reverse(Partition),[], More}, L);
	true ->
	    finalise(T, Best, L)
    end;
%% these two cases are when we manage to fit the paragraph
finalise([{Cost,[],Ws,Partition}|T], none, L) ->
    finalise(T, {Cost, reverse(Partition), Ws, []}, L);
finalise([{Cost,[],Ws,Partition}|T], Best={C,_,_,_}, L) ->
    if 
	Cost < C ->
	    finalise(T, {Cost, reverse(Partition),Ws,[]}, L);
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
    
-record(q,{minSpaceStretch,maxSpaceStretch}).

%% break_line(Toks, Len) -> [{Cost,Before,After}] sorted by Cost
%%  Toks = Before = After = [inline()]
%%  Len     = width of paragraph in points
%%  NOTES: 
%%      1) NLs have been replace by spaces PRIOR to calling this routine.
%%      2) The paragraph will only be split at a space 
%%         or in the middle of a hyphenatable word
%%      3) If the line fits then there is only one alternative
%%  Method:
%%    1) change all NL's into spaces
%%    2) compute the first before such that length(Before--Spaces) > Len
%%    3) Iterate 

break_line(Toks, Len) ->
    %% just make a quick check to see if the entire line fits
    L = case lineWidth(Toks) of
	    K when K < Len*1000 ->
		[{0,{Toks,[]}}];
	    _ ->
		Q = #q{maxSpaceStretch=150, minSpaceStretch=70},
		break_line(Toks, Len, Q)
	end,
    %% dbg_io("break_line|~s|~n=~p~n",
    %% [r2s(Toks),map(fun({Cost, {L1,L2}}) -> 
    %% {Cost, r2s(L1),r2s(L2)} end, L)]),
    L.

break_line(Toks, Len, Q) ->
    %% dbg_io("break_line Len=~p measure=~p PointSize=~p~n",
    %% [Len,Measure,PointSize]),
    {Before, After} = worse_break_line(Toks, Len),
    %% dbg_io("Worse break line: Before=|~s|~nAfter=|~s|~n",[r2s(Before),
    %% r2s(After)]),
    L = break_before(reverse(Before), After, Len, Q, []),
    filter_candidates(L, Len, Q).

%% worse_break_line(Toks, Len) -> {Before, After}
%%   Find the worse possible break of a line when the
%%   line has so much data that the size of all the characters excluding the
%%   spaces is greater than the required length.

%%   
%% Split Toks into {Before, After} when the length of
%% all the data in Before (excluding spaces) is greater than Len.
%% split *only* at a space. After splitting the After will begin with
%% a space (or be [])

worse_break_line(Toks, Len) -> worse_break_line(Toks, Len*1000, []).

worse_break_line(All=[H|T], Len, L) ->
    %% len is the length of the line in milli points
    case is_space(H) of
	true ->
	    case widthExcludingSpaces(L) of
		Len1 when Len1 > Len ->
		    {reverse(L), All};
		_ ->
		    worse_break_line(T, Len, [H|L])
	    end;
	false ->
	    worse_break_line(T, Len, [H|L])
    end;
worse_break_line([], Len, L) ->
    {reverse(L), []}.

filter_candidates(Partitions, Len, Q) ->
    P1 = map(fun({Before, []}) ->
		     %% If After is zero and the line fits then it is a 
		     %% perfect fit
		     case lineWidth(Before) of
			 K when K < Len*1000 ->
			     {0, {Before,[]}};
			 _ ->
			     {badness(Before, Len, Q), {Before, []}}
		     end;
		({Before, After}) ->
		     {badness(Before, Len, Q), {Before, After}};
		(X) ->
		     dbg_io("uugh=~p~n",[X]),
		     aaaa
	     end, Partitions),
    %% dbg_io("P1=~p~n",[map(fun({C,{I,J}}) -> {C,r2s(I),r2s(J)} end, P1)]),
    P2 = filter(fun({-1000000, _}) -> false;
		   ({_,{[],_}}) -> false;
		   (_) -> true end, P1),
    %% dbg_io("P2=~p~n",[P2]),
    sort(fun({I,_},{J,_}) -> I*I < J*J end, P2).

%% break_before(Before, After, Len, Q, ..
%%   This breaks up the before sequence by moving objects onto the
%%   after sequence. This makes the line gapper and gappier.
%%   The before segment has the words in reverse order
%%   So normally before looks like
%%   [{wd2,..},{sp2, ...} - if it begins with a space we have
%%   a little problem

%% break_before(Before, After, Len, Q, L)

%% This bit is tricky the after segment starts with a space OR []
%% The starting point is
%%    
%%    Bn ...  B3 B2 B1 B0 | S A0 A1 ...
%%    
%%    Split this thus
%%  
%%    Bn ...  Bk S | M | S A0 A1
%%   
%%    M is called a segement. A segment is a sequence of items that
%%  must be moved as a whole. This is to maintain the invarient that
%%  we can only split on a space.

%%  1) If B0 = S => impossible
%%
%%    2)  If B0 is a word then hyphenate in all possible
%%        ways add into B0[1] and B0[2] to either side of the
%%        partition forming:
%%
%%        Bn ...  B3 B2 B1 B0[1] | [B0[2] S A0 A1 ...
%%
%%    3)  Now run shift_until_space

%%    2)  Can we move all of B0 onto After
%%        Yes if B1 = space
%%        No otherwise

%% There are two invarients in this loop
%% 1) Before does not begin with a space
%% 2) After  does begin with a space

break_before([],After,Len,Q,L) ->
    check_after_invarient(After),
    L;
break_before(Before,After,Len,Q,L0) ->
    check_before_invarient(Before),
    check_after_invarient(After),
    %% Before is in reverse order
    %% dbg_io("Break_before:~s~n",[r2s(Before)]),
    B = badness(Before, Len, Q),
    %% dbg_io("Badness=~p~n", [B]),
    case B of
	%% stop when the line is very loose
	Finite when Finite > 40, length(L0) > 10 ->
	    L0;
	_ ->
	    %% Given Bn.....B0
	    %% Split into two segments
	    %% {Sement, S2} such that Segment contains no blanks
	    %% then {Bn.....S, ... B0}
	    {Segment, Before1} = extract_segment(Before, []),
	    %% Segment is the list of things that can be
	    %% shifted into the After space
	    %% Segment is in normal order
	    %% Before1 *begins* with a space or is []
	    %% dbg_io("Segment=~p~n",[Segment]),
	    Ps = make_partitions(Segment),
	    %% dbg_io("partitions=~p~n",[Ps]),
	    %% Ps = [{Bs,As}]
	    %% Now the final {B,A} pairs
	    %% are formed from
	    %% {Bs++Before1, As++After}
	    %% 
	    L1 = map(fun({Bs,As}) ->
			     {reverse(reverse(Bs)++Before1), As++After}
		     end, Ps),
	    %% After begun with a space which we remove in
	    %% the final list
	    L2 = [{reverse(Before), remove_leading_spaces(After)}|L1] ++ L0,
	    After1 = reverse(Segment, After),
	    case Before1 of
		[] -> L2;
		[Space|Before2] ->
		    break_before(Before2, [Space|After1], Len, Q, L2)
	    end
    end.

check_before_invarient(Before=[H|_]) ->
    case is_space(H) of
	true ->
	    dbg_io("Invarient broken before begines with a space:"
		      "~p~n",[Before]),
            ok;
	false ->
	    true
    end.

check_after_invarient([]) -> true;
check_after_invarient(After=[H|_]) ->
    case is_space(H) of
	true -> true;
	false -> dbg_io("invarient broken after begins:~p~n",[After])
    end.	    
    
%% make_partitions(M) -> [{Before, After}]
%%    M is a list of inlines that does not contain a space or a newline
%%    it must be split by hyphenating all words in M in all possible
%%    ways. M is in normal order.

make_partitions(M) ->  make_partitions(M, [], []).

make_partitions([H|T], Before, Final) ->
    case is_word(H) of
	true ->
	    Ps = all_hyphenations(H),
	    L1 = map(fun({X1,X2}) ->
			     {reverse([X1|Before]), [X2|T]}
		     end, Ps),
	    make_partitions(T, [H|Before], L1 ++ Final);
	false ->
	    make_partitions(T, [H|Before], Final)
    end;
make_partitions([], _, Final) ->
    Final.

%% extract segment x y S a b c -> {[x,y], [S,a,b,c]}
 
extract_segment(A=[H|T], L) ->
    case is_space(H) of
	true ->
	    {reverse(L), A};
	false ->
	    extract_segment(T, [H|L])
    end;
extract_segment([], L) ->
    {reverse(L), []}.

%% all_hyphenations(Word) -> [{Word1, Word2}].

all_hyphenations(Word) ->
    %% extract the string from the word
    Str = string(Word),
    {Str1, Tail} = remote_trailing_stuff(Str, []),
    Ps = eg_hyphenate:partitions(Str1),
    map(fun({A,B}) ->
		StrA = A ++ "-",
		StrB = B ++ Tail,
		Word1 = cloan_word(Word, StrA),
		Word2 = cloan_word(Word, StrB),
		{Word1, Word2}
	end, Ps).

remote_trailing_stuff([H|T], L) when H >= $a, H =< $z ->
    remote_trailing_stuff(T, [H|L]);
remote_trailing_stuff([H|T], L) when H >= $A, H =< $Z ->
    remote_trailing_stuff(T, [H|L]);
remote_trailing_stuff(X, L) ->
    {reverse(L), X}.

%% The badness of the line
%% ActualWidth = size of line with zero spaces
%% Len         = Required length of the line
%% NBlanks     = #places to distribute the blanks 
%%
%% To compute the badness of a line
%%   we compute the actual length (including blanks)
%%   and the size of the blanks themselves


badness(Line, Len0, Q) ->
    %% dbg_io("Badness of:~s~n",[r2s(Line)]),
    %% DataWidth = width of line (not including blanks)
    DataWidth= widthExcludingSpaces(Line),
    Len = Len0 * 1000,
    %% dbg_io("Data Width=~p Len=~p Str=~s~n",[DataWidth, Len,r2s(Line)]),
    B = if 
	    Len - DataWidth < 1 ->
		%% It won't fit at all this is a catastrophy
		%% give a large negative badness
		-1000000;
	    true ->
		%% It will fit
		%% We compute how much to stretch or shrink
		%% the blanks in the line
		%% Len          = required width
		%% DataWidth    = width of data alone, i.e. no blanks
		Width = spaces_width(Line),
		%% Width   = The "natural width" of all the spaces
		if Width < 1 ->
			-1000000;
		   true ->
			R = (Len - DataWidth)/Width,
			%% dbg_io("R=~p~n",[R]),
			badness(R, Q)
		end
	end,
    %% dbg_io("Badness=~p~n",[B]),
    B.
	    
%% R = strechyness of blank
badness(R, Q) ->
    Max = Q#q.maxSpaceStretch/100,
    Min = Q#q.minSpaceStretch/100,
    B = if 
	    R < 0.1 ->
		-100000;
	    R < Min ->
		Alpha = -100000/(Min*Min*Min*Min),
		Beta = -1,
		T = (Min-R),
		Alpha*T*T*T*T + Beta;
	   true ->
		%% R > 1
		T = (R - 1)/(Max - 1),
		T*T
	end,
    %% dbg_io("Nominal size of blank=~p actual=~p Min=~p Max=~p B=~p~n",
    %% [Nominal,Need, Min, Max, B]),
    B.

%% Toks = [tok()]
%%   tok() = {sp2,Index,W} | {wd2,Index,W,Str}

%% spaces_info(inline()) ->
%%   {NBlanks, Width}

spaces_width(Toks) ->
    Toks1 = filter(fun(I) -> is_space(I) end, Toks),
    lineWidth(Toks1).

r2s(Toks) -> richText2str({richText, Toks}).
