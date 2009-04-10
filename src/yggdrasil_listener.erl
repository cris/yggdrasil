-module(yggdrasil_listener).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	     terminate/2, code_change/3]).

-export([start_link/0]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 4990).

-record(state, {
		listener, % Listening socket
		acceptor, % Asynchronous acceptor's internal reference
		module    % FSM handling module
	}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc  Called by a supervisor to start the listening process.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
init(_Opts) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("listener: init ~p", [self()]),
	{Host, Port} = {?DEFAULT_HOST, ?DEFAULT_PORT},
	{ok, IpAddr} = inet:getaddr(Host, inet),
	error_logger:info_msg("IP:: ~p", [IpAddr]),
	SockOpts = [binary, {ip, IpAddr}, {packet, 0},
		{reuseaddr, false}, {active, false}],
    case gen_tcp:listen(Port, SockOpts) of
    {ok, ListenSocket} ->
        %%Create first accepting process
        {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
        {ok, #state{listener = ListenSocket,
                    acceptor = Ref,
                    module   = ?MODULE}};
    {error, Reason} ->
        {stop, Reason}
    end.

%%-------------------------------------------------------------------------
%% Func: handle_call(Request, From, State) -> 
%%                                      {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Purpose: Handling call messages
%% @private
%%-------------------------------------------------------------------------
handle_call({hi, Name}, _From, State) ->
	error_logger:info_msg("listener: hi ~p~n", [Name]),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc  Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc  Handling all non call/cast messages
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref, module=_Module} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
            ok              -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,

        %% New client connected - spawn a new process
		{ok, Pid} = yggdrasil_receiver:start_link(),
        gen_tcp:controlling_process(CliSocket, Pid),
        yggdrasil_receiver:set_socket(Pid, CliSocket),

        %% Signal the network driver that we are ready
		%% to accept another connection
        case prim_inet:async_accept(ListSock, -1) of
            {ok,    NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListSock, Ref, Error},
			#state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(Info, State) ->
    error_logger:error_msg("Unexpected experience: ~p.\n", [Info]),
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec terminate(Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
	gen_tcp:close(State#state.listener),
    ok.

%%-------------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Taken from prim_inet.  We are merely copying some socket options from
%% the listening socket to the new client socket.
%% @private
%%-------------------------------------------------------------------------
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
