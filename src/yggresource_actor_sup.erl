-module(yggresource_actor_sup).
-author('cris.kiev@gmail.com').

-include("yggdrasil.hrl").

-behaviour(supervisor).
-export([init/1]).

-export([
        start_link/0
    ]).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
%%----------------------------------------------------------------------
start_link() -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
%%----------------------------------------------------------------------
init([]) ->
    Actor =
    {yggresource_actor,
        {yggresource_actor, start_link, []},
        temporary,
        brutal_kill,
        worker,
        [yggresource_actor]
    },
    {ok, {{simple_one_for_one, 10, 10}, [Actor]}}.
