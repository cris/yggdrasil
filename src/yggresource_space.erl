-module(yggresource_space).
-author('cris.kiev@gmail.com').

-include("yggdrasil.hrl").

-behaviour(gen_fsm).
-export([init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4]).

%% FSM states
-export([
        'WAIT'/2,
        'WAIT'/3
]).

%% API
-export([
        start_link/1
    ]).

-record(state, {
        actors = [],
        master
    }).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
%%-------------------------------------------------------------------------
%% @spec (MasterPid) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link(MasterPid) ->
    gen_fsm:start_link(?MODULE, MasterPid, []).

%%%----------------------------------------------------------------------
%%% Callbacks for gen_fsm
%%%----------------------------------------------------------------------
%%-------------------------------------------------------------------------
%% Func: init(Args)
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init(MasterPid) ->
    {ok, 'WAIT', #state{actors=[MasterPid], master=MasterPid}}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3 (Event, StateName, StateData)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4 (Event, From, StateName, StateData)
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%-------------------------------------------------------------------------
%% FSM states
%%-------------------------------------------------------------------------
%%-------------------------------------------------------------------------
%% Func: StateName/2 (Request, State)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT'(_Request, State) -> 
    error_logger:info_msg("Area(2) is active.~p\n", [State]),
    {next_state, 'WAIT', State}.

%%-------------------------------------------------------------------------
%% Func: StateName/3 (Request, From, State)
%% Returns: {reply, Reply, NextStateName, NewStateData} |
%%          {reply, Reply, NextStateName, NewStateData, Timeout} |
%%          {reply, Reply,  NextStateName,  NewStateData,  hibernate} |
%%          {next_state, NextStateName, NewStateData} |
%%          {next_state, NextStateName, NewStateData, Timeout} |
%%          {next_state, NextStateName, NewStateData, hibernate} |
%%          {stop, Reason, Reply, NewStateData} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT'({request, Request}, _From, State) -> 
    error_logger:info_msg("Actor in game.~p\n", [State, Request]),
    {reply, ok, 'WAIT', State}.

%%%------------------------------------------------------------------------
%%% private API
%%%------------------------------------------------------------------------
