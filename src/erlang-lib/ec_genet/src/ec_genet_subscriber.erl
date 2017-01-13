-module(ec_genet_subscriber).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
-include("ec_genet.hrl").
-include("ec-genet-config.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    %% FIXME: read initial config, using empty for now
    subscriber(),
    ConfState = #state{},
    eclog(info, "subscriber started", []),
    {ok, ConfState}.

%%--------------------------------------------------------------------
%% handle_call(ping, _From, State) ->
%%     Reply = pong,
%%     {reply, Reply, State};
handle_call(Req, _From, State) ->
    eclog(error, "Got unexpected call: ~p", [Req]),
    Reply = error,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    eclog(error, "Got unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(Info, State) ->
    eclog(error, "Got unexpected info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    eclog(info, "Server stopped - ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%
%%% Internal
%%%%%%%%%

eclog(error, Format, Args) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: " ++ Format, [?SERVER | Args]);
eclog(info, Format, Args) ->
    econfd:log(?CONFD_LEVEL_INFO,  "~p: " ++ Format, [?SERVER | Args]);
eclog(trace, Format, Args) ->
    econfd:log(?CONFD_LEVEL_TRACE, "~p: " ++ Format, [?SERVER | Args]).

%%%%%%%%%
%% Log config retrieval
%%%%%%%%%

subscriber() ->
    %% FIXME: unsupervised; port hard-wired
    update_conf(?NCS_PORT),
    {ok, SubscriberSock} = econfd_cdb:connect({127,0,0,1}, ?NCS_PORT),
    {ok, Sub} = econfd_cdb:subscribe_session(SubscriberSock),
    {ok, _} = econfd_cdb:subscribe(Sub, 1, "/ec-genet/logging"),
    ok = econfd_cdb:subscribe_done(Sub),
    proc_lib:spawn_link(fun() -> subscribe_loop(?NCS_PORT, Sub) end).

subscribe_loop(Port, Sub) ->
    try
        econfd_cdb:wait(Sub, 20000, fun(Update) -> reader(Update, Port) end)
    catch X:Y ->
            eclog(error, "ERR reader died ~p~n", [{X,Y}])
    end,
    subscribe_loop(Port, Sub).

-define(LOGPATH, [logging, [?genet__ns_uri|'ec-genet']]).

reader([Point], Port) ->
    update_conf(Port),
    ?CDB_DONE_PRIORITY.

update_conf(Port) ->
    {ok, RSock} = econfd_cdb:connect({127,0,0,1}, Port),
    {ok, Cdb} = econfd_cdb:new_session(RSock, ?CDB_RUNNING),
    {ok, ?CONFD_ENUM_VALUE(LevelNum)} = econfd_cdb:get_elem(Cdb, [level | ?LOGPATH]),
    {ok, Output} = econfd_cdb:get_elem(Cdb, ['log-file-name' | ?LOGPATH]),
    Level = case LevelNum of
                ?genet_error -> error;
                ?genet_note -> note;
                ?genet_warn -> warn;
                ?genet_debug -> debug;
                ?genet_info -> info;
                _ -> off  % will cause no logging
            end,
    gen_server:cast(ec_genet_logger, {update, Level, Output}),
    econfd_cdb:close(Cdb).
