%%======================================================================
%%   General Terms
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Joe Armstrong
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

-module(erlguten_afm).

%% Version 2 ...
%% Parse an afm file and record the following information

-include("erlguten.hrl").
 
%% We need the following parameters. Most (but not all) 
%% of these can be found by
%% parsing the .fm
%% 
%% BaseFont    = The name of the Font (This is the exact name)
%% FirstChar   = First Char in the widths table (derived from the widths)
%% LastChar    = Last  Char in the widths table 
%% Ascender    = value of Ascender  in .afm file
%% CapHeight   = value of CapHeight in .afm file
%% Descender   = value of Descender in .afm fil},
%% Flags       = see notes
%% FontBBox    = value of FontBox in .afm (four integers)
%% ItalicAngle = value of ItalicAngle in .afm
%% StemV       = see notes 
%% XHeight     = value of XHeight in .afm
%% 
%% Special parameters.
%% 

%% StemV is NOT in the .afm file but *is* in the
%% encrypted part of the .pfb file where it is set with a command like
%% /StdVW[50]def - this is the dominanmt width of the vertical stems
%% (see page 42 of the Adobe "black book")
%% 
%% The  entry  StdVW  is an  array  with  only  one real  number  entry
%% expressing   the   dominant   width   of  vertical   stems   (measured
%% horizontally in  character space units). Typically, this  will be the
%% width of  straight stems  in lower case  letters. (For an  italic font
%% program,  give the width  of the  vertical stem  measured at  an angle
%% perpendicular to the stem direction.) For example: /StdVW [85] def

%% Page 42 of the Black book
%% Flags (sanserif/serif, fixed-pitch, symbolic, script, all-cap, 
%%       small-cap etc)
%% You can ask Acrobat Distiller in such cases by having it distill a 
%% simple PS file like below, with compression turned off, 
%% and then examining the PDF file
%% 
%% 	    %!PS-Adobe-2.0
%% 	    /Palatino-Roman findfont 16.0 scalefont setfont
%% 	    40 700 moveto (Palatino-Roman 16.0 point) show
%% 	    showpage


%% afm parser (quick and dirty)
%% Just look for the stuff I'm interested in and assume a fixed format for 
%% input

-compile(export_all).

-import(lists, [foreach/2, keysearch/3, map/2, foldl/3,reverse/1]).

fonts_in() ->
    "/usr/share/fonts/afms/adobe".

fonts_out() ->
    this_dir() ++ "/fonts/efm".

this_dir() ->
    filename:dirname(code:which(?MODULE)).

font_map() ->
    fonts_out() ++ "/adobe.map".

%% Just run pdf_afm_qdh:all() to build the font tables

all() ->
    F = find:files(fonts_in(), "*.afm", false),
    FontMap = map(fun parse/1, F),
    File = font_map(),
    io:format("FontMap(~s)~n~p~n",[File,FontMap]),
    file:write_file(File, term_to_binary(FontMap)).

read_font_info(FontName) ->
    %% io:format("Font map=~s~n",[font_map()]),
    case file:read_file(font_map()) of
	{ok, Bin} ->
	    T = binary_to_term(Bin),
	    case keysearch(FontName, 1, T) of
		{value, {_,File}} ->
		    FontFile = fonts_out() ++ "/" ++ File ++ ".efm",
		    case file:read_file(FontFile) of
			{ok, Bin1} ->
			    All = binary_to_term(Bin1),
			    Char_widths=mk_widths(All),
			    {ok, {afm_qdh1, 
				  All#afm2.baseFont,
				  Char_widths,
				  All#afm2.kernPairs,
				  All}};
			_ ->
			    {error, {misssing_font_file, FontFile}}
		    end;
		_ ->
		    {error, no_such_font}
	    end;
	_ ->
	    {error, no_font_map}
    end.

available_fonts() ->
    case file:read_file(font_map()) of
	{ok, Bin} ->
	    T = binary_to_term(Bin),
	    map(fun(I) -> element(1, I) end, T);
	_ ->
	    []
    end.

parse() ->
    parse("tir.afm").

parse(F) ->
    io:format("Parsing:~p~n",[F]),
    Root = filename:basename(filename:rootname(F)),
    Out = fonts_out() ++ "/" ++ Root ++ ".efm",
    L = file_utils:file2numbered_lines(F),
    Fn   = get_font_name(L),
    io:format("Found foun:~s~n",[Fn]),
    Cw = get_char_widths(L),
    Kern = get_kerning_info(L, Cw),

    Cw1  = map(fun({Index,Width,_Name}) ->
		      {Index, Width}
	      end, Cw),
    %% io:format("Cw1=~p~n",[Cw1]),
    {First,Last,Widths} = normalise_widths(Cw1),
    io:format("First=~p last=~p ~n",[First,Last]),
    Kern1 = map(fun({XY,_,W}) -> {XY, W} end, Kern),
    Ascender = get_val(L, "Ascender"),
    T = #afm2{baseFont=Fn, widths=Widths, firstChar=First,
	      lastChar=Last, kernPairs=Kern1,
	      ascender=get_val(L, "Ascender"),
	      capHeight=get_val(L, "CapHeight"),
	      descender=get_val(L, "Descender"),
	      italicAngle=get_val(L, "ItalicAngle"),
	      xHeight=get_val(L,"XHeight"),
	      flags=flags(Fn),
	      stemV=stemV(Fn),
	      fontBBox=get_fontBBox(L)},
    file:write_file(Out, term_to_binary(T)),
    io:format("Font ~s ~w entries ~w entries in kerning table~n",
	      [Fn, length(Cw1), length(Kern1)]),
    {Fn, Root}.


normalise_widths(Pairs) ->
    P1 = lists:sort(Pairs),
    {First,Width} = hd(P1),
    {Last, Ws} = gather(First, P1, []),
    {First,Last, Ws}.

gather(X, [{X,W}], L) ->
    {X, reverse([W|L])};
gather(X, [{X,W}|T], L) ->
    gather(X+1, T, [W|L]);
gather(X, Z=[{Y,W}|T], L) when Y > X ->
    gather(X+1,Z,[0|L]).

get_fontBBox(L) ->
    T = get_keyword(L, "FontBBox "),
    [F1,F2,F3,F4|_] = string:tokens(T, "\s\r\n"),
    {list_to_integer(F1),list_to_integer(F2),list_to_integer(F3),
     list_to_integer(F4)}.

get_font_name(L) ->
    T = get_keyword(L, "FontName "),
    [F|_] = string:tokens(T, "\s\r\n"),
    F.

get_val(L, Tag) ->
    T = get_keyword(L, Tag ++" "),
    [F|_] = string:tokens(T, "\s\r\n"),
    %% io:format("Tag=~s T=~s F=~s~n",[Tag, T, F]),
    case lists:member($., F) of
	true ->
	    list_to_float(F);
	false ->
	    list_to_integer(F)
    end.

get_keyword([{_,Str}|T], Prefix) ->
    case is_prefix(Prefix, Str) of
	{yes, T1} ->
	    T1;
	no ->
	    get_keyword(T, Prefix)
    end;
get_keyword([], Prefix) ->
    io:format("Cannot find:~s setting to 0~n",  [Prefix]),
    "0".

is_prefix([], L) ->
    {yes, L};
is_prefix([H|T], [H|T1]) ->
    is_prefix(T, T1);
is_prefix(_, _) ->
    no.
    
get_kerning_info(L, C) ->
    foldl(fun(I, Acc) ->
		  case parse_kerning(I, C) of
		      {ok, Add} ->
			  [Add|Acc];
		      no ->
			  Acc
		  end
	  end, [], L).

parse_kerning({_,Str="KPX " ++ _}, Cw) ->
    case string:tokens(Str, "\s\r\n") of
	["KPX",C1,C2,W] ->
	    case charno(C1, Cw) of
		{ok, N1} ->
		    case charno(C2, Cw) of
			{ok, N2} ->
			    {ok, {{N1,N2}, {C1, C2}, list_to_integer(W)}};
			_ ->
			    no
		    end;
		_ ->
		    no
	    end;
	Other ->
	    io:format("UUgh:~s:~p~n",[Str,Other]),
	    no
    end;
parse_kerning(_, Cw) ->
    no.

charno(C, Cw) ->
    case [I || {I,_,Name} <- Cw, Name == C] of
	[N] ->
	    {ok, N};
	_ ->
	    io:format("Cannot locate character:~p~n",[C]),
	    error
    end.

get_char_widths(L) ->
    foldl(fun add_char/2, [], L).

add_char({Line,Str= "C " ++ _}, Acc) ->
    case parse_char_data(Str) of
	{-1,_,_} ->
	    Acc;
	V ->
	    [V|Acc]
    end;
add_char(_, Acc) ->
    Acc.

parse_char_data(S) ->
    case string:tokens(S, "\s\r\n") of
	["C",C,";","WX",W,";","N",Name,";"|_] ->
	    {list_to_integer(C), list_to_integer(W),Name};
	Other ->
	    io:format("wot is:~s:~pn",[S, Other])
    end.

		      




%% FontDescriptor Flags 
%% Bit       32-20        19       18
%% Meaning  reserved  force bold  small ..... not completed
%%                 when small   cap
%% (Note: MSB is bit-32, LSB is bit-1)


flags("Helvetica") -> 32;
flags("Helvetica-Bold") -> 0;
flags("Helvetica-Oblique") -> 96;
flags("Helvetica-BoldOblique") -> 0;
flags("Times-Roman") -> 0;
flags("Times-Bold") -> 0;
flags("Times-Italic") -> 0;
flags("Times-BoldItalic") -> 0;
flags("Courier") -> 0;
flags("Courier-Bold") -> 0;
flags("Courier-Oblique") -> 0;
flags("Courier-BoldOblique") -> 0;
flags("Symbol") -> 0;
flags("ZapfDingbats") -> 0;
flags("AvantGarde-Book") -> 32;
flags("AvantGarde-BookOblique") -> 96;
flags("AvantGarde-Demi") -> 262176;
flags("AvantGarde-DemiOblique") -> 262240;
flags("Bookman-Demi") -> 262178;
flags("Bookman-DemiItalic") -> 262242;
flags("Bookman-Light") -> 34;
flags("Bookman-LightItalic") -> 98;
flags("Helvetica-Narrow") -> 32;
flags("Helvetica-Narrow-Oblique") -> 96;
flags("Helvetica-Narrow-Bold") -> 262176;
flags("Helvetica-Narrow-BoldOblique") -> 262240;
flags("NewCenturySchlbk-Roman") -> 34;
flags("NewCenturySchlbk-Italic") -> 98;
flags("NewCenturySchlbk-Bold") -> 262178;
flags("NewCenturySchlbk-BoldItalic") -> 262242;
flags("Palatino-Roman") -> 34;
flags("Palatino-Italic") -> 98;
flags("Palatino-Bold") -> 262178;
flags("Palatino-BoldItalic") -> 262242;
flags("ZapfChancery-MediumItalic") -> 98;
flags("Helvetica-Condensed") -> 32;
flags("Helvetica-Condensed-Bold") -> 262176;
flags("Helvetica-Condensed-Oblique") -> 96;
flags("Helvetica-Condensed-BoldObl") -> 262240;
flags(_) -> 0.

%% stemV
stemV("Helvetica") -> 0;
stemV("Helvetica-Bold") -> 0;
stemV("Helvetica-Oblique") -> 0;
stemV("Helvetica-BoldOblique") -> 0;
stemV("Times-Roman") -> 0;
stemV("Times-Bold") -> 0;
stemV("Times-Italic") -> 0;
stemV("Times-BoldItalic") -> 0;
stemV("Courier") -> 0;
stemV("Courier-Bold") -> 0;
stemV("Courier-Oblique") -> 0;
stemV("Courier-BoldOblique") -> 0;
stemV("Symbol") -> 0;
stemV("ZapfDingbats") -> 0;
stemV("AvantGarde-Book") -> 70;
stemV("AvantGarde-BookOblique") -> 70;
stemV("AvantGarde-Demi") -> 133;
stemV("AvantGarde-DemiOblique") -> 133;
stemV("Bookman-Demi") -> 167;
stemV("Bookman-DemiItalic") -> 172;
stemV("Bookman-Light") -> 96;
stemV("Bookman-LightItalic") -> 96;
stemV("Helvetica-Narrow") -> 88;
stemV("Helvetica-Narrow-Oblique") -> 88;
stemV("Helvetica-Narrow-Bold") -> 140;
stemV("Helvetica-Narrow-BoldOblique") -> 140;
stemV("NewCenturySchlbk-Roman") -> 92;
stemV("NewCenturySchlbk-Italic") -> 80;
stemV("NewCenturySchlbk-Bold") -> 154;
stemV("NewCenturySchlbk-BoldItalic") -> 150;
stemV("Palatino-Roman") -> 84;
stemV("Palatino-Italic") -> 84;
stemV("Palatino-Bold") -> 122;
stemV("Palatino-BoldItalic") -> 122;
stemV("ZapfChancery-MediumItalic") -> 70;
stemV("Helvetica-Condensed") -> 79;
stemV("Helvetica-Condensed-Bold") -> 130;
stemV("Helvetica-Condensed-Oblique") -> 79;
stemV("Helvetica-Condensed-BoldObl") -> 130;
stemV(X) -> 
    io:format("I dont't know about:~p~n", [X]),
    70.

mk_widths(M) ->
    First = M#afm2.firstChar,
    W     = M#afm2.widths,
    mk_widths(First, W).

mk_widths(N, [])    -> [];
mk_widths(N, [0|T]) -> mk_widths(N+1,T);
mk_widths(N, [W|T]) -> [{N,W}|mk_widths(N+1,T)].
    

    


