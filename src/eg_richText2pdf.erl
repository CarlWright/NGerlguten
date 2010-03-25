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

-module(eg_richText2pdf).

%% There is a bug in this code: Blanks *inside*
%% strings are subject to expansion caused by the Tw operator

-export([richText2pdf/8]).

-import(lists, [map/2,reverse/1, reverse/2]).

-import(pdf_op, [a2s/1, i2s/1, n2s/1, f2s/1, flatten/1]).
-import(eg_richText, [classify_inline/1, string/1, font/1, pointSize/1,
		      color/1, string/1, lineWidth/1, numberOfSpaces/1]).

-record(pdf, {color=default,
	      xy={-1,-1},face={none,none},tw=-1,inTJ=false,code=[]}).

%% -define(DEBUG, true).

-ifdef(DEBUG).
dbg_io(Str) -> dbg_io(Str,[]).
dbg_io(Str,Args) ->
    io:format("eg_richText2pdf: ~p " ++ Str, [self()] ++ Args),
    ok.
-else.
dbg_io(_) -> ok.
dbg_io(_,_) -> ok.
-endif.

richText2pdf(X, Y0, Type, Rot, Lines, Leading, Widths, Offsets) ->
    Y = Y0 - Leading,
    P = start(),
    P2 = case Type of
	     justified ->
		 {Cos, Sin, P1} = init_rotation_matrix(X, Y0, Rot, P),
		 make_justified(X,Y,Leading,Lines,Offsets,Widths,P1);
	     Style when Style == left_justified;
			Style == right_justified;
			Style == centered ->
		 {Cos, Sin, P1} = init_rotation_matrix(X, Y0, Rot, P),
		 make_para(X, Y, Leading, Lines,Offsets,Widths,Style,P1)
    end,
    finalise(P2).

make_justified(X, Y, Leading, [], _, _, P) -> P;
make_justified(X, Y, Leading, [H], [O|_], [W|_], P) -> 
    line2pdf(X+O,Y,H,W,last_line_justified, P);
make_justified(X, Y, Leading, [H|T], [O|O1] = O0, [W|W1] = W0, P) -> 
    {O2, W2} = last_offset_width(O0, W0),
    P1 = line2pdf(X+O,Y,H,W,justified,P),
    make_justified(X, Y-Leading, Leading, T, O2, W2, P1).

make_para(X, Y, Leading, [], _, _, _, P) -> P;
make_para(X, Y, Leading, [H], [O|_], [W|_], Style, P) -> 
    line2pdf(X+O,Y,H,W,Style, P);
make_para(X, Y, Leading, [H|T], [O|O1] = O0, [W|W1] = W0, Style, P) -> 
    {O2, W2} = last_offset_width(O0, W0),
    P1 = line2pdf(X+O,Y,H,W,Style,P),
    make_para(X, Y-Leading, Leading, T, O2, W2, Style, P1).

last_offset_width([O|O1],[W|W1]) ->
    O2 = if O1 == [] -> [O];
            true     -> O1
         end,
    W2 = if W1 == [] -> [W];
            true     -> W1
         end,
    {O2, W2}.

line2pdf(X, Y, {richText, Line}, Len, Style, P) ->
    TotWidth = lineWidth(Line)/1000,
    NS       = numberOfSpaces(Line),
    case Style of
	justified ->
	    Tw = if 
		     NS > 0 ->
			 Width = (Len - TotWidth)/NS,
			 Width;
		     NS == 0 ->
			 0
		 end,
	    %% dbg_io("Line=~p~n",[Line]),
	    %% dbg_io("NS=~p Tw=~p Len=~p TotWidth=~p~n",
	    %% [NS,Tw,Len,TotWidth]),
	    make_line(X, Y, Line, Tw, P);
	last_line_justified ->
	    %% The last line of a justfied para has to be handled with care
	    %% It might happen that the line needs to be squashed ...
	    if 
		TotWidth > Len ->
		    line2pdf(X, Y, {richText, Line}, Len, justified, P);
		true ->
		    line2pdf(X, Y, {richText, Line}, Len, left_justified, P)
	    end;
	left_justified ->
	    make_line(X, Y, Line, 0, P);
	right_justified ->
	    Excess = Len - TotWidth,
	    make_line(X+Excess, Y, Line, 0, P);
	centered ->
            %% dbg_io("Len = ~p~n",[Len]),
            %% dbg_io("TotWidth = ~p~n",[TotWidth]),
	    Offset = round(Len - TotWidth) div 2,
            
            %% dbg_io("Offset = ~p~n",[Offset]),
	    make_line(X+Offset, Y, Line, 0, P)
    end.

make_line(X, Y, Line, Tw, P) ->
    %% dbg_io("make line~p at pos=~p ~p~n",[Line,X,Y]),
    P1 = add_move(X, Y, P),
    make_line(Line, Tw, P1).

make_line([H|T], Tw, P) ->
    case classify_inline(H) of
	space ->
	    {Font,Size} = get_font_info(H),
	    P1 = ensure_face(Font, Size, P),
	    P2 = ensure_tw(Tw, P1),
	    make_line(T, Tw, add_space(Font, P2));
	word ->
	    {Font,Size} = get_font_info(H),
	    Str = string(H),
	    %% dbg_io("Outputting Word=~p Font=~p oldFont=~p~n",
	    %% [H,Font,P#pdf.face]),
	    P1 = ensure_face(Font, Size, P),
	    P2 = ensure_tw(Tw, P1),
	    Color = color(H),
	    P3 = ensure_color(Color, P2),
	    make_line(T, Tw, add_string(Font, Str, P3));
	fixedStr ->
	    {Font,Size} = get_font_info(H),
	    Str = string(H),
	    P1 = ensure_face(Font, Size, P),
	    P2 = ensure_tw(0, P1),
	    make_line(T, Tw, add_string(Font, Str, P2));
	_ ->
	    dbg_io("Don't know how to make a line with a: ~p~n",[H]),
	    make_line(T, Tw, P)
    end;
make_line([], _, P) ->
    P.

get_font_info(X) ->
    {font(X), pointSize(X)}.

finalise(P) ->
    P1 = close_tj(P),
    reverse(P1#pdf.code).

start() -> #pdf{}.

init_rotation_matrix(X, Y, 0, P) ->
    %% dbg_io("here=~p~n",[{X,Y,0,P}]),
    C = "1 0 0 1 " ++ n2s(X) ++ " " ++ n2s(Y) ++ " Tm ",
    P1 = add_code(C, P#pdf{xy={X,Y}}),
    {1,0,P1};
init_rotation_matrix(X, Y, Rot, P) ->
    Rads = 3.14159*Rot/180,
    Cos = math:cos(Rads),
    Sin = math:sin(Rads),
    C = n2s(Cos) ++ " " ++ n2s(Sin) ++ " " ++ n2s(-Sin) ++ " " ++
	n2s(Cos) ++ " " ++ n2s(X) ++ " " ++ n2s(Y) ++ " Tm ",
    P1 = add_code(C, P#pdf{xy={X,Y}}),
    {Cos, Sin, P1}.

add_move(X, Y, P) ->
    P1 = close_tj(P),
    case P1#pdf.xy of
	{-1, -1} ->
	    %% dbg_io("Here aaa*********~n"),
	    C = "1 0 0 1 " ++ n2s(X) ++ " " ++ n2s(Y) ++ " Tm ",
	    add_code(C, P1#pdf{xy={X,Y}});
	{X, Y} ->
	    add_code("0 0 TD ", P1);
	{OldX, OldY} ->
	    Dx = X - OldX,
	    Dy = Y - OldY,
	    C = n2s(Dx) ++ "  " ++ n2s(Dy) ++ " TD ",
	    add_code(C, P1#pdf{xy={X,Y}})
    end.

ensure_face(Font, Pts, P) ->
    case P#pdf.face of
	{Font, Pts} ->
	    P;
	{_, _} ->
	    P1 = close_tj(P),
	    P2 = P1#pdf{face={Font,Pts}},
	    Index = Font:index(),
	    add_code("/F" ++ i2s(Index) ++ " " ++ i2s(Pts) ++ " Tf ", P2)
    end.

ensure_color(Color, P) ->
    case P#pdf.color of
	Color ->
	    P;
	_ ->
	    P1 = close_tj(P),
	    P2 = P1#pdf{color=Color},
	    Code = set_color(Color),
	    add_code(Code, P2)
    end.


set_color(default) ->
    set_color({0,0,0});
set_color({R,G,B}) ->
    f2s(R) ++ " " ++ f2s(G) ++" " ++ f2s(B) ++ " rg ".

close_tj(P) ->
    case P#pdf.inTJ of
	true ->
	    add_code("] TJ ", P#pdf{inTJ=false});
	false ->
	    P
    end.

open_tj(P) ->
    case P#pdf.inTJ of
	false ->
	    add_code("[", P#pdf{inTJ=true});
	true ->
	    P
    end.

ensure_tw(N, P) ->
    case P#pdf.tw of
	N -> P;
	_ ->
	    P1 = close_tj(P),
	    add_code(n2s(N) ++ " Tw ", P1#pdf{tw=N})
    end.

add_string(Font, Str, P) ->
    %% font etc are set
    P1 = open_tj(P),
    Code = str2pdf(Font, Str),
    add_code(Code, P1).

add_space(Font, P) ->
    add_string(Font,  " ", P).

add_code(Str, P) ->
    C1 = P#pdf.code,
    P#pdf{code=reverse(Str, C1)}.


str2pdf(Font, "")  -> "";
str2pdf(Font, Str) ->
    K = str2TJ(Font, Str),
    K1 = map(fun({Str1,Kern}) -> {quote_strings(Str1), Kern} end, K),
    Pdf1 = map(fun({S,I}) -> ["(", S,")",i2s(I)] end, K1),
    flatten(Pdf1).
    
quote_strings([$(|T])  -> [$\\,$(|quote_strings(T)];
quote_strings([$)|T])  -> [$\\,$)|quote_strings(T)];
quote_strings([$\\|T]) -> [$\\,$\\|quote_strings(T)];
%%quote_strings([$"|T])  -> [$\\,$"|quote_strings(T)];
quote_strings([H|T])   -> [H|quote_strings(T)];
quote_strings([])      -> [].

%% str2TJ(Font, Str) -> [{str(),int()}].

str2TJ(Font, Str) ->
    str2TJ(Font, Str, [], []).

str2TJ(Font, [32,H|T], Tmp, L) ->
    str2TJ(Font, [H|T], [32|Tmp], L);
str2TJ(Font, [H,32|T], Tmp, L) ->
    str2TJ(Font, T, [32,H|Tmp], L);
str2TJ(Font, [H1,H2|T], Tmp,  L) ->
    case Font:kern(H1, H2) of
	0 ->
	    str2TJ(Font, [H2|T], [H1|Tmp], L); 
	N ->
	    Str = reverse([H1|Tmp]),
	    str2TJ(Font, [H2|T], [], [{Str,-N}|L])
    end;
str2TJ(Font, [H|T], Tmp, L) ->
    str2TJ(Font, T, [H|Tmp], L);
str2TJ(Font, [], [], L) ->
    reverse(L);
str2TJ(Font, [], Tmp, L) ->
    reverse([{reverse(Tmp), 0}|L]).

%% To set the font use /Fn Pt Tf
%% [(A) 90 (W) 120 (A) 105 (Y again - correctly kerned) ] TJ
