-module(ec_genet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    GenetOwner = {ec_genet_owner, {ec_genet_owner, start_link, []},
                  Restart, Shutdown, Type, [ec_genet_owner]},
    GenetChild = {ec_genet_server, {ec_genet_server, start_link, []},
                  Restart, Shutdown, Type, [ec_genet_server]},
    LoggerChild = {ec_genet_logger, {ec_genet_logger, start_link, []},
                  Restart, Shutdown, Type, [ec_genet_logger]},
    LogConfSubChild = {ec_genet_logconf, {ec_genet_subscriber, start_link, []},
                  Restart, Shutdown, Type, [ec_genet_subscriber]},
    {ok, {SupFlags, [GenetOwner, GenetChild, LoggerChild, LogConfSubChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
