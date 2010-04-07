%%%-------------------------------------------------------------------
%%% File    : erlguten_sup.erl
%%% Author  :  <wright@servicelevel.net>
%%% Description : 
%%%
%%% Created :  1 Mar 2010 by  <wright@servicelevel.net>
%%%-------------------------------------------------------------------

-module(erlguten_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    ErlgutenChild = {font_server,{erlguten_font_server,start_link,[]},
        permanent,2000,worker,[erlguten_font_server, erlguten_afm]},
    {ok,{{one_for_all,0,1}, [ErlgutenChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
