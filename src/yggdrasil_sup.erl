-module(yggdrasil_sup).
-author('cris.kiev@gmail.com').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() -> 
    error_logger:info_msg("Supervisor!"),
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    io:format("Result: ~p~n", [Result]),
    Result.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    ActorSup = 
    {yggdrasil_actor_sup,
        {yggdrasil_actor_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [yggdrasil_actor_sup]
    },
    ReceiverSup = 
    {yggdrasil_receiver_sup,
        {yggdrasil_receiver_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [yggdrasil_receiver_sup]
    },
    Listener =
    {yggdrasil_listener,
        {yggdrasil_listener, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [yggdrasil_listener]
    },
    {ok, {{one_for_one, 10, 10}, [ActorSup, ReceiverSup, Listener]}}.
