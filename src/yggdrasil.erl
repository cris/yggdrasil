-module(yggdrasil).
-author('cris.kiev@gmail.com').

-behaviour(application).
-export([start/2, start/0, stop/1]).

start() -> 
	yggdrasil_sup:start_link().

start(_Args) -> 
	start().

start(_Type, _Args) ->
	start().

stop() ->
	ok.

stop(_State) ->
	stop().
