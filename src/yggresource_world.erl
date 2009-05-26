-module(yggresource_world).
-author('cris.kiev@gmail.com').

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
        start_link/0,
        actor_call/2,
        actor_call/3
    ]).

-record(state, {
        guests = [],
        actors = [],
        spaces = []
    }).

%% General API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

actor_call(Verb, Resource) -> 
    actor_call(Verb, Resource, {}).

actor_call(Verb, Resource, Params) -> 
    gen_server:call(yggresource_world, {actor, Verb, Resource, Params}).


%%


init(_Opts) ->
    {ok, #state{spaces = [], guests = [], actors = []}}.

handle_call({'PUT', actors, {Login, Password}}, ActorPid, State) ->
    case Reply = check_actor(Login, Password) of
        ok ->
            NewState = update_state({guest_to_actor, ActorPid}, State);
        {change_actor, ActorPid} ->
            NewState = update_state({remove_guest, ActorPid}, State);
        _ ->
            NewState = State
    end,
    error_logger:info_msg("World: 'PUT' actors: ~p~n", [Reply]),
    {reply, Reply, NewState};

handle_call({actor, Verb, Resource, Params}, ActorPid, State) ->
    {Reply, StateUpdates} = actor_action(Verb, Resource, Params, ActorPid),
    NewState = update_state(StateUpdates, State),
    error_logger:info_msg("World: ~p ~p: ~p~n", [Verb, Resource, Reply]),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("Unexpected experience: ~p.\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
check_actor(Login, Password) ->
    if 
        Login =:= <<"user">> andalso Password =:= <<"pass">> ->
            ok;
        true -> 
            {error, incorrect_credentials}
    end.

% stub
check_permission(add_space, _ActorPid) ->
    ok.
%%% All actions
actor_action(_Verb, _Resource, _Params, ActorPid) ->
    StateUpdate = case Reply = check_permission(add_space, ActorPid) of
        ok ->
            {ok, NewSpace} = supervisor:start_child(yggresource_actor_sup, [ActorPid]),
            {add_space, NewSpace};
        _ ->
            none
    end,
    {Reply, StateUpdate}.

%%%------------------------------------------------------------------------
%%% private API
%%%------------------------------------------------------------------------
update_state([StateUpdate|TStateUpdates], State) ->
    NewState = update_state(StateUpdate, State),
    update_state(TStateUpdates, NewState);

update_state([], State) ->
    State;

update_state(none, State) ->
    State;

update_state({guest_to_actor, GuestPid}, State) ->
    update_state([{remove_guest, GuestPid}, {add_actor, GuestPid}], State);

update_state({remove_guest, GuestPid}, State) ->
    NGuestList = lists:delete(GuestPid, State#state.guests),
    State#state{guests=NGuestList};

update_state({add_space, ActorPid}, State) ->
    NSpaceList = [ActorPid | State#state.spaces],
    State#state{spaces=NSpaceList};

update_state({add_actor, ActorPid}, State) ->
    NActorList = [ActorPid | State#state.guests],
    State#state{actors=NActorList}.
