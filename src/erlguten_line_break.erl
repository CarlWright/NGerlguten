%%======================================================================
%% line breaker
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

-module(erlguten_line_break).

-import(lists, [append/1, map/2, filter/2, foldl/3, foreach/2,
		reverse/1, sort/2, sum/1]).
-import(erlguten_font_server, [char_width/2]).

-export([lineWidth/1, dataWidth/1, sizeof/2, split_para/3, toks2str/1]).

%% -compile(export_all).

-record(q,{minSpaceStretch,maxSpaceStretch}).

%% split_para(Toks, Measure, PtSize)
%%  Tok     = {wd2,Tag,Bool,Width,Str} | {sp2, Index, Width}
%%  Measure = measure of the paragraph in Picas
%%  PtSize  = size of typeface in Points

split_para(Toks, Measure, PtSize) ->
    %% io:format("break_line:~p width:~p Pts:~p ~n", [Toks, Measure, PtSize]),
    Q = #q{maxSpaceStretch=115, minSpaceStretch=95},
    break_line(Toks, Measure, PtSize, Q).

break_line(Toks, Measure, PointSize, Q) ->
    Len = trunc(12000*Measure/PointSize),
    %% io:format("break_line Len=~p measure=~p PointSize=~p~n",
    %% [Len,Measure,PointSize]),
    {Before, After} = first_break_line(Toks, Len),
    %% io:format("FirstBreak |~s|~s|~n",[toks2str(Before),toks2str(After)]),
    L0 = [{Before,After}],
    L1 = break_before(reverse(Before), After, Len, Q, L0),
    L2 = break_after(reverse(Before), After, Len, Q, L1),
    %% io:format("L2=~p~n",[L2]),
    filter_candidates(L2, Len, Q).

filter_candidates(Partitions, Len, Q) ->
    P1 = map(fun({Before, []}) ->
		     %% If After is zero and the line fits then it is a 
		     %% perfect fit
		     case lineWidth(Before) of
			 K when K < Len ->
			     {0, {Before,[]}};
			 _ ->
			     {badness(Before, Len, Q), {Before, []}}
		     end;
		({Before, After}) ->
		     {badness(Before, Len, Q), {Before, After}}
	     end, Partitions),
    %% io:format("P1=~p~n",[P1]),
    P2 = filter(fun({infinite, _}) -> false;
		   ({_,{[],_}}) -> false;
		   (_) -> true end, P1),
    %% io:format("P2=~p~n",[P2]),
    sort(fun(I,J) -> I < J end, P2).

%% break_before(Before, After, Len, Q, ..
%%   This breaks up the before sequence moving objects onto the
%%   after sequence. This maks the line gapper and gappier.
%%   The before segment has the words in reverse order
%%   So normally before looks like
%%   [{wd2,..},{sp2, ...} - if it begins with a space we have
%%   a little problem

break_before([],After,Len,Q,L) ->
    L;
break_before(Before,After,Len,Q,L0) ->
    %% Here we move objects on the Before segment onto
    %% The After segment
    %% Note that After does not begin with a blank
    %% Nor does Before so we have to add additional spaces
    %% as we add things to before and After
    B = badness(Before, Len, Q),
    %% io:format("Badness=~p~n", [B]),
    case B of
	infinite -> L0; %% this case should not happen ..
	Finite when Finite > 150 ->
	    L0;
	_ ->
	    [Word|Before1] = Before,
	    Before2 = remove_spaces(Before1),
	    {wd2,Font,Splittable,_,Str} = Word,
	    Wspace = mk_space(Font),
	    L1 = [{reverse(Before2), [Word,Wspace|After]}|L0],
	    Rest = case Splittable of
		       false ->
			   [];
		       true ->
			   case hyphenate_partitions(Str) of
			       [] -> [];
			       Partitions ->
				   %% io:format("Hypenating:~p => ~p~n",
				   %% [Str, Partitions]),
				   map(fun({Bs,As}) ->
					       Bs1 = Bs ++ "-",
					       W1 = sizeof(Font, Bs1),
					       W2 = sizeof(Font, As),
					       {reverse([{wd2,Font,
							  Splittable,W1,Bs1},
							 Wspace|Before1]),
						[{wd2,Font,Splittable,W2,As},
						 Wspace|After]}
				       end, Partitions)
			   end
		   end,
	    L2 = L1 ++ Rest,
	    break_before(Before2, [Word,Wspace|After], Len, Q, L2)
    end.

break_after(Before, [], _Len, _Q, L) ->
    L;
break_after(Before, After, Len, Q, L0) ->
    %% We break up the After segment moving lines onto the 
    %% Before segment
    %% Here we adding extra items from the after line until
    %% the badness of the before line becomes infinite
    B = badness(Before, Len, Q),
    case B of
	infinite -> L0;
	Finite   ->
	    [Word|After1] = After,
	    After2 = remove_spaces(After1),
	    {wd2,Font,Splittable,_,Str} = Word,
	    Wspace = mk_space(Font),
	    L1 = [{reverse([Word,Wspace|Before]), After2}|L0],
	    Rest = case Splittable of
		       false ->
			   [];
		       true ->
			   case hyphenate_partitions(Str) of
			       [] -> [];
			       Partitions ->
				   %% io:format("Hypenating:~p => ~p ~n",
				   %% [Str, Partitions]),
				   map(fun({Bs,As}) ->
					       Bs1 = Bs ++ "-",
					       W1 = sizeof(Font, Bs1),
					       W2 = sizeof(Font, As),
					       {reverse([{wd2,Font,Splittable,
							  W1,Bs1},
							 Wspace|Before]),
						[{wd2, Font,Splittable,W2,As},
						 Wspace|After2]}
				       end, Partitions)
			   end
		   end,
	    L2 = L1 ++ Rest,
	    break_after([Word,Wspace|Before], After2, Len, Q, L2)
    end.

hyphenate_partitions(Str) ->
    {Str1, Tail} = remote_trailing_stuff(Str, []),
    Ps = erlguten_hyphenate:partitions(Str1),
    map(fun({A,B}) ->
		{A, B++ Tail}
	end, Ps).

remote_trailing_stuff([H|T], L) when H >= $a, H =< $z ->
    remote_trailing_stuff(T, [H|L]);
remote_trailing_stuff([H|T], L) when H >= $A, H =< $Z ->
    remote_trailing_stuff(T, [H|L]);
remote_trailing_stuff(X, L) ->
    {reverse(L), X}.

%% The badness of the line
%% ActualWidth = size of line with zero spaces
%% Len         = Actual length of line
%% NBlanks     = #places to distribute the blanks 
%%
%% To compute the badness of a line
%%   we compute the actual length (including blanks)
%%   and the size of the blanks themselves
%%    
badness(Line, Len, Q) ->
    %% io:format("Badness of:~s~n",[toks2str(Line)]),
    %% DataWidth = width of line (not including blanks)
    DataWidth= dataWidth(Line),
    %% io:format("Data Width=~p Measure=~p~n",[DataWidth, Len]),
    B = if 
	    Len - DataWidth < 1 ->
		%% It won't fit at all this is a catastrophy
		infinite;
	    true ->
		%% It will fit
		%% We compute how much to stretch or shrink
		%% the blanks in the line
		%% Len          = required width
		%% DataWidth    = width of data alone, i.e. no blanks
		{NBlanks, Width} = blanks_info(Line),
		case NBlanks of
		    0 ->
			0;
		    _ ->
			%% io:format("Nblanks=~p~n",[NBlanks]),
			Bs = (Len - DataWidth)/NBlanks,
			W = Width/NBlanks,
			badness_blank(W, Bs, Q)
		end
	end,
    %% io:format("Badness=~p~n",[B]),
    B.
	    
%% Required = required size of a single blank
%% Actual   = actual size of a blank
badness_blank(Required, Actual, Q) ->
    Max = Actual*Q#q.maxSpaceStretch/100,
    Min = Actual*Q#q.minSpaceStretch/100,
    badness_blank(Required, Min, Actual,  Max).

badness_blank(Required, Min, Actual,  Max) ->
    B = if Required > Actual ->
		T = ((Required-Actual)/(Max-Actual)),
		T*T;
	   true ->
		2*(1/Required-1/Actual)/(1/Min-1/Actual)
	end,
    %% io:format("Blank=~p nominal=(~p, ~p, ~p) badness=~p~n",
    %% [Required,Min,Actual,Max,B]),
    B.

%% Toks = [tok()]
%%   tok() = {sp2,Index,W} | {wd2,Index,W,Str}

first_break_line(Toks, Len)  -> first_break_line(Toks, Len, 0, []).

first_break_line(After=[H|T], Measure, Len, Before) ->
    Len1 = Len + tok2width(H),
    %% io:format("Fist breakline Len=~p H=~p Len1=~p Measure=~p~n",
    %% [Len1, Len, H, Measure]),
    if 
	Len1 > Measure ->
	    %% This trims away any blanks at the start of After
	    %% of the end of Before
	    {reverse(remove_spaces(Before)), remove_spaces(After)};
       true ->
	    first_break_line(T, Measure, Len1, [H|Before])
    end;
first_break_line([], _, _, Before) ->
    {reverse(remove_spaces(Before)), []}.

remove_spaces([{sp2,_,_}|T]) -> remove_spaces(T);
remove_spaces(X)             -> X.
    
%%----------------------------------------------------------------------
%% sizeof(FontIndex, Str)
%%   Computes width of Str which is of type FontIndex
%%   Size is correctly adjusted for kerning information

sizeof(FontIndex, Str) ->
    Widths = map(fun(I) -> char_width(FontIndex, I) end, Str),
    %% io:format("Str=|~s| Widths=~p~n",[Str, Widths]),
    W1 = sum(Widths),
    %% and add the correct kerning info
    kern_adj(Str, W1, FontIndex).

kern_adj([H1,H2|T], W, FontIndex) ->
    Extra = erlguten_font_server:kern(FontIndex, {H1,H2}),
    if Extra == 0 ->
	    true;
       true ->
	    %% io:format("Kern: ~p ~p  =~p ~n", [H1, H2, Extra])
	    true
    end,
    kern_adj([H2|T], W+Extra, FontIndex);
kern_adj(_, W, _) ->
    W.

%%----------------------------------------------------------------------

tok2width({wd2,_,_,W,_}) -> W;
tok2width({sp2,_,W})   -> W.
    
tok2str({wd2,_,_,_,Str}) -> Str;
tok2str({sp2,_,_})     -> " ".

toks2str(L) ->    
    lists:flatten(map(fun tok2str/1, L)).

lineWidth(Toks) ->
    foldl(fun(I, S) -> tok2width(I) + S end, 0, Toks).

dataWidth(Toks) ->
    Toks1 = filter(fun is_word/1, Toks),
    foldl(fun(I, S) -> tok2width(I) + S end, 0, Toks1).

blanks_info(Toks) ->
    Toks1 = filter(fun is_blank/1, Toks),
    {length(Toks1), lineWidth(Toks1)}.

is_blank({sp2,_,_}) -> true;
is_blank(_)         -> false.

is_word({wd2,_,_,_,_}) -> true;
is_word(_)             -> false.

mk_space(Font) ->
    {sp2, Font, char_width(Font, 32)}.

    
		  






