-module(yggdrasil_receiver_sup).
-author('cris.kiev@gmail.com').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).

%%----------------------------------------------------------------------
%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
%%----------------------------------------------------------------------
start_link() -> 
	error_logger:info_msg("Receiver supervisor"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
%%----------------------------------------------------------------------
init([]) ->
	Receiver =
	{yggdrasil_receiver,
		{yggdrasil_receiver, start_link, []},
		temporary,
		brutal_kill,
		worker,
		[yggdrasil_receiver]
	},
	{ok, {{simple_one_for_one, 10, 10}, [Receiver]}}.
