%%%-------------------------------------------------------------------
%%% File    : pdf.erl
%%% Author  :  <wright@servicelevel.net>
%%% Description : 
%%%
%%% Created : 12 April 2010 by  <wright@servicelevel.net>
%%%-------------------------------------------------------------------
-module(newpdf).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).
   
-export([new/1]).

-include("../include/eg.hrl").

-define(SERVER, newpdf).

-record(state, {}).

new(Pid) ->
    {{Year,Month,Day},{Hrs,Min,Sec}} = calendar:local_time(),
    PDFC = #pdfContext{info=#info{creator="Erlang", 
  			    creationDate= {{Year,Month,Day},{Hrs,Min,Sec}},
  			    producer="NGerlguten", 
  			    author="",
  			    title="",
  			    subject="",
  			    keywords="ErlangKeyword"},
  		      images=dict:new(),
  		      fonts=[],
  		      currentpage=1,
  		      mediabox=pagesize(a4)},
      gen_server:call(Pid,     
        PDFC,infinity).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
handle_call({info,Info}, _From, PDFC) ->
  NewInfo = handle_PDF_info(PDFC#pdfContext.info, Info),
  NewPDFC = PDFC#pdfContext{info=NewInfo},
  {reply, ok, NewPDFC};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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

% set fields that characterize a PDF document.

handle_PDF_info(I,{author,Author})->
    I#info{author=Author};
handle_PDF_info(I,{title,Title}) ->
    I#info{title=Title};
handle_PDF_info(I,{subject,Subject}) ->
    I#info{subject=Subject};
handle_PDF_info(I,{date,{Year,Month,Day}})->
    I#info{creationDate={Year,Month,Day}};
handle_PDF_info(I,{keywords,Keywords}) ->
    I#info{keywords=Keywords}.


%% --- Page ---
pagesize(a0)             -> pagesize( 2380, 3368 );
pagesize(a1)             -> pagesize( 1684, 2380 );
pagesize(a2)             -> pagesize( 1190, 1684 );
pagesize(a3)             -> pagesize( 842, 1190 );
pagesize(a4)             -> pagesize( 595, 842 );
pagesize(a5)             -> pagesize( 421, 595 );
pagesize(a6)             -> pagesize( 297, 421 );
pagesize(a7)             -> pagesize( 210, 297 );
pagesize(a8)             -> pagesize( 148, 210 );
pagesize(a9)             -> pagesize( 105, 148 );
pagesize(b0)             -> pagesize( 2836, 4008 );
pagesize(b1)             -> pagesize( 2004, 2836 );
pagesize(b2)             -> pagesize( 1418, 2004 );
pagesize(b3)             -> pagesize( 1002, 1418 );
pagesize(b4)             -> pagesize( 709, 1002 );
pagesize(b5)             -> pagesize( 501, 709 );
pagesize(b6)             -> pagesize( 355, 501 );
pagesize(b7)             -> pagesize( 250, 355 );
pagesize(b8)             -> pagesize( 178, 250 );
pagesize(b9)             -> pagesize( 125, 178 );
pagesize(b10)            -> pagesize( 89, 125 );
pagesize(c5e)            -> pagesize( 462, 649 );
pagesize(comm10e)        -> pagesize( 298, 683 );
pagesize(dle)            -> pagesize( 312, 624 );
pagesize(executive)      -> pagesize( 542, 720 );
pagesize(folio)          -> pagesize( 595, 935 );
pagesize(ledger)         -> pagesize( 1224, 792 );
pagesize(legal)          -> pagesize( 612, 1008 );
pagesize(letter)         -> pagesize( 612, 792 );
pagesize(tabloid)        -> pagesize( 792, 1224 ).

pagesize(Width, Height) -> {0,0,Width,Height}.