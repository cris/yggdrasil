-module(yggresource_world).
-author('cris.kiev@gmail.com').

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

-export([
        actors/3
    ]).

-record(state, {
        guests,
        actors,
        areas
    }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Opts) ->
    {ok, #state{areas = [], guests = [], actors = []}}.

handle_call({'PUT', actors}, _From, State) ->
    error_logger:info_msg("listener: hi ~p~n", [Name]),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("Unexpected experience: ~p.\n", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
actors('PUT', Params, _Request) ->
    {Login, Password} = yggdrasil_utils:get([login, password], Params),
    error_logger:info_msg("Route: server/actors: ~p\n", [Login]),
    check_credentials(Login, Password).


%% private API
check_credentials(Login, Password) ->
    if 
        Login =:= <<"user">> andalso Password =:= <<"pass">> ->
            {ok, ActorPid} = supervisor:start_child(yggdrasil_actor_sup, []),
            {actor, ActorPid};
        true -> 
            guest
    end.


