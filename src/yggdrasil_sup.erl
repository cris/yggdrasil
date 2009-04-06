-module(yggdrasil_sup).
-author('cris.kiev@gmail.com').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() -> 
	error_logger:info_msg("Supervisor!"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.
