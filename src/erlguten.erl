%%======================================================================
%% Main program
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
%% Last Edit: 2003-03-12
%% =====================================================================

-module(erlguten).

-compile(export_all).
-import(lists, [foreach/2, map/2]).
-import(pdf, [flatten/1]).

-record(box, {continue,  % str()  = name of the continuation frame 
	      free=1,    % int()  = first free line in the box
	      grid,      % bool   = show a grid
	      bg,        % no | {yes,R,G,B} background color
	      pointSize, % int()  = size in points of the main text in
	                 %          the box
	      leading,   % int()  = gap between lines in points
	      maxLines,  % int()  = max lines in the box
	      measure,   % int()  = width of box in picos (1 pico=12 points)
	      name,      % str()  = name of box
	      objs,      % [#obj] = objects
	      x,         % int()  = X coord of top left hand corner of box
	      y}).       % int()  = Y coord of top left hand corner of box

-record(obj,{
	  name,          % atom() = tag name
	  paraIndent,    % {int,int} = {first,line,...}
	  tags}).        % [#tag]

batch([X]) ->
    format(atom_to_list(X)).

test() ->
    format("test1.xml").

format(File) ->
    V = erlguten_xml_lite:parse_file(File),
    %% io:format("read:~p~n",[V]),
    Out = filename:rootname(File) ++ ".pdf",
    case V of
	{error, W} ->
	    io:format("Error in source:~p~n",[V]),
	    exit(1);
	[{pi,_},{xml,{document,_, Flows}}] ->
	    PDF  = pdf:new(),
	    foreach(fun({flow,Args,Data}) ->
			    Box = parse_flow(Args),
			    format_flow(PDF, Data, Box)
		    end, Flows),
	    Serialised = pdf:export(PDF),
	    file:write_file(Out,[Serialised]),
	    pdf:delete(PDF);
	_ ->
	    io:format("bad XML - must begin \"<?xml ...\n<flow \n"),
	    exit(1)
    end.

format_flow(PDF, Chunks, Box) ->
    %% io:format("Flow Chunks:~p into box:~p~n", [Chunks, Box]),
    Box1 = Box#box{free=1},
    #box{x=X,y=Y,leading=Leading,measure=Measure,maxLines=Max} = Box1,
    case Box1#box.bg of
	no ->
	    void;
	{yes,{R,G,B}} ->
	    pdf:save_state(PDF),
	    pdf:set_fill_color_RGB(PDF,0.9,0.8,0.6),
	    pdf:rectangle(PDF, X-5,Y-Leading-5,10+Measure*12,10+Leading, fill),
	    pdf:restore_state(PDF)
    end,
    format_flow1(Chunks, Box1, PDF),
    case Box1#box.grid of
	true ->
	    P = erlguten_geometry:draw_box(X,Y,Measure,Leading,Max),
	    pdf:append_stream(PDF, P);
	false ->
	    void
    end.

format_flow1([Xml={Tag,Args,Data}|T], Box1, PDF) ->
    %% io:format("Flow Chunk:~n~p~n into box:~n~p~n", [Xml, Box1]),
    CurrentObj = get_tag_schema(atom_to_list(Tag), Box1#box.objs),
    TagMap = CurrentObj#obj.tags,
    %% io:format("Tag schema=~p~n", [CurrentObj]),
    %% io:format("Tag map=~p~n", [TagMap]),
    %% make a font_map
    %% Something like ..
    %% [{raw,1,true,"Times-Roman"},{em,2,true,"Times-Italic"},..]
    FontMap = map(fun({Tg,Name,Bool}) ->
			  {Tg,pdf:get_font_alias(PDF, Name),Bool,Name}
		  end, TagMap),
    %% io:format("FontMap=~p~n", [FontMap]),
    P1 = CurrentObj#obj.paraIndent,
    Measure = Box1#box.measure, 
    %% io:format("ParaIndent=~p Measure=~p~n", [P1, Measure]),
    ParaShape = map(fun(I) -> Measure-I end, P1),
    %% io:format("ParaShape=~p~n", [ParaShape]),
    PointSize = Box1#box.pointSize,
    %% io:format("PointSize=~p~n", [PointSize]),
    Toks  = erlguten_normalise_xml:normalise_xml(Xml, FontMap),
    Lines = erlguten_para_break:break_para(Toks, PointSize, ParaShape, FontMap),
    PdfLines = erlguten_lines2pdf:lines2pdf(Lines, PointSize, ParaShape, FontMap),
    %% Now figure out if we can fit the paragraph in this page
    Need = length(Lines),
    Free = Box1#box.free,
    Max  = Box1#box.maxLines,
    Available = Max - Free + 1,
    %% io:format("I need ~p lines there are ~p~n", [Need, Available]),
    case Need =< Available of 
	true ->
	    %% io:format("Good no worries~n"),
	    #box{x=X,y=Y,leading=Leading,measure=Measure} = Box1,
	    Y1 = Y-Leading - (Free-1)*Leading,
	    Geom = erlguten_geometry:mk_line_headers(X, Y1, Measure, 
						Leading, ParaShape, Need),
	    Pdf1 = ["BT\n", zip1(Geom, PdfLines), "ET\n"],
	    Pdf2 = flatten(Pdf1),
	    pdf:append_stream(PDF, Pdf2),
	    Box2 = Box1#box{free=Free+Need},
	    format_flow1(T, Box2, PDF);
	false ->
	    %% io:format("Oh dear~n")
	    void
    end;
format_flow1([H|T], Box, Pdf) ->
    io:format("wot is this:~p~n",[H]),
    format_flow1(T, Box, Pdf);
format_flow1([], _, _) ->
    true.
    
get_tag_schema(Tag, [H|T]) ->
    case H#obj.name of
	Tag -> H;
	_   -> get_tag_schema(Tag, T)
    end;
get_tag_schema(Tag, []) ->
    exit({missing,tag,Tag}).

parse_flow([{"galley",F},{"name",Tag}]) ->
    case erlguten_xml_lite:parse_file(F) of
	{error, E} ->
	    io:format("Error in galley(~p):~p~n",[F, E]),
	    exit(1);
	L ->
	    G = parse_galley(F, L),
	    get_box(Tag, G)
    end.

get_box(Tag, {galley,_,Boxes}) ->
    %% io:format("Here:~p ~p~n",[Tag, Boxes]),
    get_box1(Tag, Boxes).

get_box1(Tag, [H|T]) ->
    case H#box.name of
	Tag -> H;
	_   -> get_box1(Tag, T)
    end;
get_box1(Tag, []) ->
    exit({missing,box,Tag}).

parse_galley(F, [{pi,_},{xml, {galley,[],Boxes}}]) ->
    {galley, F, map(fun parse_box/1, Boxes)}.

parse_box({box, [{"bg", Col},
		 {"continue",C},
		 {"fontSize",F},
		 {"grid", Grid},
		 {"lines",L},
		 {"measure",M},
		 {"name",Name},
		 {"x",X},
		 {"y",Y}], Objs}) ->
    {Pt,Leading} = parse_fontSize(F),
    Lines = parse_int("lines", L),
    Measure = parse_int("measure", M),
    XX = parse_int("x", X),
    YY = parse_int("y", Y),
    Os  = map(fun parse_obj/1, Objs),
    Bg = parse_color(Col),
    Box = #box{continue=C,
	       grid=to_bool(Grid),
	       bg=Bg,
	       pointSize=Pt,
	       leading=Leading,
	       maxLines=Lines,
	       measure=Measure,
	       name=Name,
	       objs=Os,
	       x=XX,
	       y=YY},
    %% io:format("Box=~p~n",[Box]),
    Box;
parse_box(B) ->
    io:format("Invalid box:~p~n",[B]).

parse_obj({obj, [{"name",Name},{"paraIndent",P}], Tags}) ->
    P1 = parse_paraIndent(P),
    PTags = map(fun parse_tag/1, Tags),
    #obj{name=Name,paraIndent=P1,tags=PTags}.

parse_tag({tag, [{"break", B},{"font",F},{"name",N}], []}) ->
    {list_to_atom(N),F,to_bool(B)};
parse_tag(Tag) ->
    io:format("Tag=~p~n",[Tag]),
    exit('EXIT bad_tag').

parse_color("default") -> no;
parse_color(Str) ->
    [R,G,B] = string:tokens(Str,","),
    {yes, {parse_float(R), parse_float(G), parse_float(B)}}.

to_bool("true") ->
    true;
to_bool("false") ->
    false;
to_bool(X) ->
    io:format("expecting true or false got:~s~n",[X]),
    exit(1).

parse_fontSize(S) ->
    case string:tokens(S, "/") of
	[A, B] ->
	    I = parse_int("fontSize",  A),
	    J = parse_int("fontSize",  B),
	    {I, J};
	_ ->
	    io:format("fontSize must be of the form <int>/<int> was:~s",[S]),
	    exit(1)
    end.

parse_paraIndent(S) ->
    case string:tokens(S, ",") of
	Toks ->
	    map(fun(I) -> parse_int("paraIndent",  I) end, Toks);
	_ ->
	    io:format("paraIndent must be of the form <int>,<int>, was:~s",
		      [S]),
	    exit(1)
    end.

parse_int(Txt, S) ->
    case (catch list_to_integer(S)) of
	{'EXIT', _} ->
	    io:format("invalid integer:~s expecting:~s~n", [S, Txt]),
	    exit(1);
	I ->
	    I
    end.

parse_float(S) ->
    case (catch list_to_float(S)) of
	{'EXIT', _} ->
	    io:format("invalid float:~s ~n", [S]),
	    exit(1);
	I ->
	    I
    end.

zip1([H1|T1],[H2|T2]) -> [[H1," ",H2]|zip1(T1, T2)];
zip1([], [])          -> [].









