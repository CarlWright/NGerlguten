%%======================================================================
%% erlguten server
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
%%%
%%% Renovated : 27 Feb 2010 by  <wright@>
%%%-------------------------------------------------------------------
-module(erlguten).

-behaviour(gen_server).

%% API
-export([start_link/0, test/1, format/2, format_string/3, dispatch/3, dispatch_string/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).
-import(lists, [foreach/2, map/2]).
-import(pdf, [flatten/1]).

-define(SERVER, erlguten).

%% this relates directly to the galley definition files
%%
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

 
-record(state, {}).


test(Pid) ->
      gen_server:call(Pid, {format, "../demos/test1.xml"}).

format(Pid, File) ->
  gen_server:call(Pid, {format, File}).

format_string(Pid, String, Root) ->
  gen_server:call(Pid, {format_string, String, Root}).

dispatch(Pid, File, To) ->
  gen_server:cast(Pid, {format, File, To}).

dispatch_string(Pid, String, Root, To) ->
  gen_server:cast(Pid, {format_string, String, Root, To}).
  
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link( ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({format_string, String, Root}, _From, State) ->
  Serialised = convert_xml_to_pdf(String, Root),
  {reply, Serialised, State};
  
handle_call({format, File}, _From, State) ->
  Out = filename:rootname(File) ++ ".pdf",
  case file:read_file(File) of
	{ok, Bin} ->
	  Root = filename:dirname(File),
	  Serialised = convert_xml_to_pdf(binary_to_list(Bin), Root),
	  file:write_file(Out,[Serialised]),
	  file:close(Out),
    {reply, ok, State};
	Error ->
	    Error
    end.



%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({format_string, String, Root, To}, State) ->
  Serialised = convert_xml_to_pdf(String, Root),
  To ! Serialised;
  
handle_cast({format, File, To}, State) ->
  Out = filename:rootname(File) ++ ".pdf",
  case file:read_file(File) of
	{ok, Bin} ->
	  Root = filename:dirname(File),
	  Serialised = convert_xml_to_pdf(binary_to_list(Bin), Root),
	  file:write_file(Out,[Serialised]),
	  file:close(Out),
    To ! ok ;
	Error ->
	    To ! Error
    end.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Given an input string of XML we produce the contents of a PDF file
%% in Serialised. We look up galley files in the location described by
%% Root.
%%--------------------------------------------------------------------

convert_xml_to_pdf(XML, Root) ->
    V = erlguten_xml_lite:parse_file(XML),  %% convert the XML into a parse tree
    case V of
	{error, _W} ->
	    io:format("Error in source:~p~n",[V]),
	    exit(1);
	[{pi,_},{xml,{document,_, Flows}}] ->
  	  PDF  = pdf:new(),                   %% begin the PDF stream
	    foreach(fun({flow,Args,Data}) ->
			    Box = parse_flow(Args, Root),   %% parse one of the flows
			    format_flow(PDF, Data, Box)     %% convert a parsed flow into PDF content
		    end, Flows),
	    Serialised = pdf:export(PDF),
	    pdf:delete(PDF),
	    Serialised;
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
	{yes,{_R,_G,_B}} ->
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
format_flow1([{raw,"\"\n"}|T], Box, Pdf) ->
    %% This consumes the empty lines in the parsed results.
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

parse_flow([{"galley",F},{"name",Tag}], Root) ->
  File = filename:join(Root, F),
  case file:read_file(File) of
	{ok, Bin} ->
    file:close(File),
    case erlguten_xml_lite:parse_file(binary_to_list(Bin)) of
  	{error, E} ->
  	    io:format("Error in galley(~p):~p~n",[F, E]),
  	    exit(1);
  	L ->
  	    G = parse_galley(F, L),
  	    get_box(Tag, G)
      end;
	Error ->
	    Error
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


