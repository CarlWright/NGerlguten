%%======================================================================
%% eg_xml_to_richText.erl
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


-module(eg_xml2richText).

-export([normalise_xml/2, normalise_xml/3, default_tagMap/1]).

-import(lists, [foldl/3, foreach/2, map/2, member/2,
		reverse/1, reverse/2, sort/2]).

-import(pdf_op, [i2s/1, f2s/1]).
-import(eg_richText, [classify_inline/1,
		      fontFromFace/1,
		      mk_fixedStr/2, mk_nl/1,
		      mk_space/1, mk_word/2, mk_face/5, is_face_breakable/1]).


%% -define(DEBUG, true).

-ifdef(DEBUG).
dbg_io(Str) -> dbg_io(Str,[]).
dbg_io(Str,Args) ->
    io:format("eg_xml2richText: ~p " ++ Str, [self()] ++ Args),
    ok.
-else.
dbg_io(_) -> ok.
dbg_io(_,_) -> ok.
-endif.

%%----------------------------------------------------------------------
%% normalise_xml(XML, RichTextTags, FontMap) ->
%%   XML = XML parse tree
%%   The tree is walked - if any Tag is in RichTextTags
%%   Then the subtree of this tag is assumend to be rich text
%%   RichTextTags = [Tag]
%%   FontMap = [#face{}]

%% Invarients no consequative spaces 
%% or spaces next to NLs

default_tagMap(Pts) -> 
    {[p],
     [{default,mk_face("Times-Roman", Pts, true, default, 0)},
      {em, mk_face("Times-Italic", Pts, true, default, 0)},
      {red, mk_face("ZapfChancery-MediumItalic", Pts, true, {1,0,0},0)},
      {blue, mk_face("ZapfChancery-MediumItalic", Pts, true, {0,0,1},0)},
      {code, mk_face("Courier", Pts, false, default, 0)},
      {b,mk_face("Times-Bold", Pts, true, default, 0)}]}.

normalise_xml(XML, {StandardTags, TagMap}) ->
    normalise_xml(XML, StandardTags, TagMap).

normalise_xml({Tag, Args, L}, RichTextTags, TagMap) ->
    case member(Tag, RichTextTags) of
	true ->
	    L1 = normalise_richText(L, TagMap),
	    {Tag, Args, L1};
	false ->
	    L1 = map(fun(I) ->
			     normalise_xml(I, RichTextTags, TagMap)
		     end, L),
	    {Tag, Args, L1}
    end;
normalise_xml(Z, _, _) ->
    dbg_io("I cannot normalise:~p~n",[Z]).

normalise_richText(Items, FontMap) ->
    L0 = foldl(fun(I, L0) -> normalise_inline(I, FontMap, L0) end, [], Items),
    L1 = reverse(L0),
    test_inline_invarient(L1),
    {richText, L1}.

test_inline_invarient([H1,H2|T]) ->    
    case {classify_inline(H1), classify_inline(H2)} of
	{space, space} ->
	    dbg_io("Warning spaces:~p ~p~n",[H1,H2]),
	    test_inline_invarient([H2|T]);
	{nl, space} ->
	    dbg_io("Warning NL + NL:~p ~p~n",[H1,H2]),
	    test_inline_invarient([H1|T]);
	{space,nl} ->
	    dbg_io("Warning spaces + NL:~p ~p~n",[H1,H2]),
	    test_inline_invarient([H2|T]);
	_ ->
	    test_inline_invarient([H2|T])
    end;
test_inline_invarient(_) ->
    true.

normalise_inline({raw,Str}, FontMap, L) ->
    normalise_tag(default, Str, FontMap, L);
normalise_inline({Tag, _, [{raw,Str}]}, FontMap, L) ->
    normalise_tag(Tag, Str, FontMap, L);
normalise_inline({Tag, _, []}, FontMap, L) ->
    L.

normalise_tag(Tag, Str, FontMap, L) ->
    Face = get_face(Tag, FontMap),
    case is_face_breakable(Face) of
	true ->
	    normalise_str(Str, Face, L, skip_ws);
	false ->
            normalise_str(Str, Face, L, keep_ws)
% 	    Wd = mk_fixedStr(Face, Str),
% 	    [Wd|L]
    end.

get_face(Tag, [{Tag,Face}|_]) -> Face;
get_face(Tag, [_|T]) -> get_face(Tag, T);
get_face(Tag, []) ->
    dbg_io("There is no face associated with Tag=~p~n",[Tag]),
    pdf:default_face().

%% Collect spaces nls etc.
%% in a breakable face
normalise_str([$\r,$\n|T], Face, L, WS) ->
    normalise_str(T, Face, [mk_nl(Face)|L], WS);
normalise_str([$\n|T], Face, L, WS) ->
    normalise_str(T, Face, [mk_nl(Face)|L], WS);
normalise_str([H|T], Face, L, WS) ->
    case {is_white(H), WS} of 
	{true, skip_ws} ->
	    %% Hop over the white space
	    %% If we get to \n put in a NL otherwise
	    %% put in a space
	    T1 = skip_white(T),
	    case T1 of
		[] ->
		    Space = mk_space(Face),
		    normalise_str(T1, Face, [Space|L], WS);
		[H2|_] ->
		    case is_nl(H2) of
			true ->
			    normalise_str(T1, Face, L, WS);
			false ->
			    Space = mk_space(Face),
			    normalise_str(T1, Face, [Space|L], WS)
		    end
	    end;
        {true, keep_ws} ->
            Space = mk_space(Face),
            normalise_str(T, Face, [Space|L], WS);
	{false, _} ->
	    {Str, T1} = collect_word(T, [H]),
	    Word = mk_word(Face, Str),
	    normalise_str(T1, Face, [Word|L], WS)
    end;
normalise_str([], _, L, WS) ->
    L.

%% End Normalise XML
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% misc

is_white($\s) -> true;
is_white($\t) -> true;
is_white(_)   -> false.

is_white_or_nl($\n) -> true;
is_white_or_nl($\r) -> true;
is_white_or_nl(X)   -> is_white(X).

is_nl($\n) -> true;
is_nl($\r) -> true;
is_nl(_)   -> false.

skip_white(X=[H|T]) ->
    case is_white(H) of
	true  -> skip_white(T);
	false -> X
    end;
skip_white([]) ->
    [].

collect_word(X=[H|T], L) ->
    case is_white_or_nl(H) of
	true  -> {reverse(L), X};
	false -> collect_word(T, [H|L])
    end;
collect_word([], L) ->
    {reverse(L), []}.









