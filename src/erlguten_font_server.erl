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
%%% Renovated : 19 Feb 2010 by  <wright@servicelevel.net>
%%%-------------------------------------------------------------------
-module(erlguten_font_server).

-behaviour(gen_server).

%% API
-export([start_link/0, available_fonts/0, ensure_loaded/2, info/1, data/1, char_width/2, kern/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).
-import(lists, [member/2, foreach/2]).

-define(SERVER, erlguten_font_server).
   
-include("../include/erlguten.hrl").
-record(state, {}).


available_fonts() ->
  Fonts = gen_server:call({global,?SERVER}, {available_fonts}),
  Fonts.

ensure_loaded(Font, Index) ->
  gen_server:call({global,?SERVER}, {ensure_loaded, Font, Index}).
  
info(Index) ->
  gen_server:call({global,?SERVER}, {info, Index}).  
  
data(Fontname) ->
  gen_server:call({global, ?SERVER}, {data, Fontname}).
  
char_width(N, Char) ->
  gen_server:call({global, ?SERVER}, {char_width, N, Char}). 

kern(N, KP) ->
  gen_server:call({global, ?SERVER}, {kern, N, KP}, infinity).
  
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

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
    case member(fonts, ets:all()) of
	true ->
	    State = true;
	false ->
	    fonts = ets:new(fonts, [named_table,set,public]),
	    State = true
    end,
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({available_fonts}, _From, State) ->
    Fonts = erlguten_afm:available_fonts(),
    {reply, Fonts, State};
    
handle_call({ensure_loaded, Font, Index}, _From, State) ->    
   % ensure_loaded(Font, Index) ->
    %% io:format("Ensure_loaded font number=~p = ~s~n", [Index, Font]),
    case ets:lookup(fonts, {info, Index}) of
	[_] ->
		  {reply, true, State};
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
		    {reply, true, State};
		{error, Why} ->
		    exit({cannot_load_font, Why})
	    end
    end;
    
handle_call({info, Index}, _From, State) -> 
    case ets:lookup(fonts, {info, Index}) of
	[{_,I}] ->
	    {reply, I, State};
	[] ->
	    exit({font_server_info,Index})
    end;
    
handle_call({data, Fontname}, _From, State) ->     
    case ets:lookup(fonts, {allData, Fontname}) of
	[{_,I}] ->
	    {reply, {ok, I}, State};
	[] ->
	    {reply, error, State}
    end;

handle_call({char_width, N, Char}, _From, State) ->  

 Module = list_to_atom("egFont" ++ pdf_op:i2s(N)),
 W = Module:width(Char),
 {reply,W, State};

   
%    case ets:lookup(fonts, {width,N,Char}) of
%	[{_,W}] ->
%	    {reply,W, State};
%	[] ->
%	    io:format("Cannot figure out width of:~p ~p~n",[N, Char]),
%	    io:format("Possible \n in code etc~n"),
%	    {reply,1000, State}
%    end;

handle_call({kern, N, KP}, _From, State) -> 
 Module = list_to_atom("egFont" ++ pdf_op:i2s(N)),
 {Char1, Char2} = KP,
 W = Module:kern(Char1, Char2),
 {reply,W, State}.
    
%    case ets:lookup(fonts, {kern,N,KP}) of
%	[{_,W}] ->
%	    {reply,W, State};
%	[] ->
%	    {reply,0, State}
%   end.

    
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

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
  ets:delete(fonts),
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

