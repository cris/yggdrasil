-module(yggdrasil_router).
-author('cris.kiev@gmail.com').

-include("yggdrasil.hrl").

-export([
        route_guest/1,
        route_actor/1
    ]).

%% dump routing implementation
route_guest(#request{resource=Resource, verb=Verb, params=Params} = Request) -> 
    {Slot, Action} = case Resource of
        <<"/server/actors">> -> {yggslot_server, actors};
            _ -> {yggslot_error, code_404}
    end,
    Slot:Action(Verb, Params, Request).

route_actor(#request{resource=Resource, verb=Verb, params=Params} = Request) -> 
    {Slot, Action} = case Resource of
        <<"/server/actors">> -> {yggslot_server, actors};
        % sample
        <<"/rooms/:room_id/users">> -> {yygslot_rooms, users};
            _ -> {yggslot_error, code_404}
    end,
    Slot:Action(Verb, Params, Request).

