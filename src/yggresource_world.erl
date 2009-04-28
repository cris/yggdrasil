-module(yggresource_world).
-author('cris.kiev@gmail.com').

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

-record(state, {
        guests = [],
        actors = [],
        areas  = []
    }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Opts) ->
    {ok, #state{areas = [], guests = [], actors = []}}.

handle_call({'PUT', actors, {Login, Password}}, ActorPid, State) ->
    case Reply = check_actor(Login, Password) of
        ok ->
            NewState = update_state(guest_to_actor, ActorPid, State);
        {change_actor, ActorPid} ->
            NewState = update_state(remove_guest, ActorPid, State);
        _ ->
            NewState = State
    end,
    error_logger:info_msg("World: 'PUT' actors: ~p~n", [Reply]),
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


%%%------------------------------------------------------------------------
%%% private API
%%%------------------------------------------------------------------------

update_state(guest_to_actor, GuestPid, State) ->
    update_state(remove_guest, GuestPid, State),
    update_state(add_actor, GuestPid, State);

update_state(remove_guest, GuestPid, State) ->
    NGuestList = lists:delete(GuestPid, State#state.guests),
    State#state{guests=NGuestList};

update_state(add_actor, ActorPid, State) ->
    NActorList = [ActorPid | State#state.guests],
    State#state{actors=NActorList}.
