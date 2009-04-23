%% Yggdrasil common record and macro definitions

-record(request, {
        verb,
        resource,
        headers,
        params,
        actor
    }).
