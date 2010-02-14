%%======================================================================
%% erlguten_font_server.erl - font server
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

-module(erlguten_font_server).

-include("erlguten.hrl").

-compile(export_all).
-import(lists, [member/2, foreach/2]).

start() ->
    case member(fonts, ets:all()) of
	true ->
	    true;
	false ->
	    fonts = ets:new(fonts, [named_table,set,public]),
	    true
    end.

stop() ->
    ets:delete(fonts).

available_fonts() ->
    erlguten_afm:available_fonts().

ensure_loaded(Font, Index) ->
    %% io:format("Ensure_loaded font number=~p = ~s~n", [Index, Font]),
    case ets:lookup(fonts, {info, Index}) of
	[_] ->
	    true;
	[] ->
	    case erlguten_afm:read_font_info(Font) of
		{ok, {afm_qdh1,Font,Widths,Kern,All}} ->
		    ets:insert(fonts, {{info, Index}, Font}),
		    ets:insert(fonts, {{allData, Font}, All}),
		    foreach(fun({Char,W}) ->
				    ets:insert(fonts,
					       {{width,Index,Char}, W})
			    end, Widths),
		    foreach(fun({KP,W}) ->
				    ets:insert(fonts,
					       {{kern,Index,KP}, W})
			    end, Kern),
		    true;
		{error, Why} ->
		    exit({cannot_load_font, Why})
	    end
    end.

info(Index) ->
    case ets:lookup(fonts, {info, Index}) of
	[{_,I}] ->
	    I;
	[] ->
	    exit({font_server_info,Index})
    end.

data(Fontname) ->
    case ets:lookup(fonts, {allData, Fontname}) of
	[{_,I}] ->
	    {ok, I};
	[] ->
	    error
    end.

char_width(N, Char) ->
    case ets:lookup(fonts, {width,N,Char}) of
	[{_,W}] ->
	    W;
	[] ->
	    io:format("Cannot figure out width of:~p ~p~n",[N, Char]),
	    io:format("Possible \n in code etc~n"),
	    1000
    end.

kern(N, KP) ->
    case ets:lookup(fonts, {kern,N,KP}) of
	[{_,W}] ->
	    W;
	[] ->
	    0
    end.









