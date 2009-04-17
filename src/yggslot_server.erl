-module(yggslot_server).
-author('cris.kiev@gmail.com').

-export([
        actors/3
    ]).

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

