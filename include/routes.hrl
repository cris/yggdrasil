-define(ROUTES,
    [
        [server, rooms],
        [{rooms,'?'}],
        [{rooms,'?'}, {actors,'?'}, messages],
        [server, {rooms,'?'}],
        [{rooms,'?'}, actions],
        [server, actors],
        [{actors,'?'}, messages]
    ]
).
-define(ROUTES_BIN,
    [
        [<<"server">>, <<"rooms">>],
        [<<"rooms">>, '?'],
        [<<"rooms">>, '?', <<"actors">>, '?', <<"messages">>],
        [<<"server">>, <<"rooms">>, '?'],
        [<<"rooms">>, '?', <<"actions">>],
        [<<"server">>, <<"actors">>],
        [<<"actors">>, '?', <<"messages">>]
    ]
).
