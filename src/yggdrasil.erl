-module(yggdrasil).
-author('cris.kiev@gmail.com').

-behaviour(application).
-export([start/2, start/0, stop/0, stop/1]).

start() ->
    application:start(yggdrasil).

stop() ->
    application:stop(yggdrasil).

start(normal, _Args) ->
    {ok, Pid} = yggdrasil_sup:start_link(),
    {ok, Pid}.

stop(_State) ->
    ok.
