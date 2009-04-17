%% Yggdrasil comman record and macro definition

-record(request, {
        verb,
        resource,
        headers,
        params,
        actor,
        socket
    }).

-record(actor, {
        login,
        socket
    }).
