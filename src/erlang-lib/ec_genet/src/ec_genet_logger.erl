-module(ec_genet_logger).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

-export([log/4]).

-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
-include("ec_genet.hrl").

-define(SERVER, ?MODULE).

-record(state, {output=none, filename=none, levels=[]}).
-define(ALL_LEVELS, [debug, info, note, warn, error]).  % ordering is important

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    %% initial config read in the subscriber
    ConfState = #state{},
    eclog(info, "logger started", []),
    {ok, ConfState}.

handle_call(getstate, _From, State) ->
    %% debugging only
    {reply, State, State};
handle_call(Request, _From, State1) ->
    State2 = process(Request, State1),
    {reply, ok, State2}.

handle_cast(Request, State1) ->
    State2 = process(Request, State1),
    {noreply, State2}.

handle_info(Info, State) ->
    eclog(error, "Got unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    eclog(info, "Server stopped - ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log(Level, Module, Line, Args) ->
    request({log, Level, Module, Line, Args}).

%%%%%%%%%
%%% Internal server request processing
%%%%%%%%%

request(Request) ->
    gen_server:cast(?SERVER, {Request, os:timestamp()}).

process({{log, Level, Module, Line, Args}, Timestamp}, State) ->
    output_log(State, Timestamp, {'',Level}, '', location(Module, Line), Args),
    State;
process({update, Level, File}, State) ->
    process_state_update(Level, binary_to_list(File), State);
process(Msg, State) ->
    eclog(error, "Got unexpected request: ~p", [Msg]),
    State.

process_state_update(Level, File, State=#state{filename=File}) ->
    State#state{levels = lists:dropwhile(fun(L) -> L =/= Level end, ?ALL_LEVELS)};
process_state_update(LN, File, State) ->
    if is_pid(State#state.output) ->
            file:close(State#state.output);
       true -> ok end,
    case file:open(File, [write, delayed_write]) of
        {ok, Dev} ->
            %% perhaps levels changed too
            process_state_update(LN, File, State#state{output=Dev, filename=File});
        Err ->
            eclog(error, "Failed to open file ~p for logging: ~p", [File, Err]),
            #state{filename=File}
    end.

eclog(error, Format, Args) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: " ++ Format, [?SERVER | Args]);
eclog(info, Format, Args) ->
    econfd:log(?CONFD_LEVEL_INFO,  "~p: " ++ Format, [?SERVER | Args]);
eclog(trace, Format, Args) ->
    econfd:log(?CONFD_LEVEL_TRACE, "~p: " ++ Format, [?SERVER | Args]).

%%%%%%%%%
%%% Logging functions
%%%%%%%%%

%% @doc Format new log entry and write it to the output file.
-spec output_log(#state{},
                 os:timestamp(),
                 {Group::atom(),Level::atom()},
                 Event::atom(),
                 {Module::atom(), Function::atom(), Line::integer()},
                 Args::[term(),...]) ->
                        ok.
output_log(State, Timestamp, {Group,Level}, Event, Location, [Format, Args]) ->
    Levels = State#state.levels,
    %%eclog(trace, ">>> ec_genet_logger Levels=~p loglevel=~p",[Levels, get(ec_genet_loglevel)]),
    LogEntry = lists:flatten(io_lib:format(Format, Args)),
    case lists:member(Level, Levels) of
        true ->
            highlight_line(State, Group, Level, Event,
                           io_lib:format("~s [~p;~p] ~s ~s",
                                         [format_timestamp(Timestamp), Level, Group, format_location(Location), LogEntry]));
        _ ->
            ok
    end.

format_location({Module, '', Line}) ->
    io_lib:format("~p:~p", [Module, Line]);
format_location({Module, Function, Line}) ->
    io_lib:format("~p:~p:~p", [Module, Line, Function]).

location(Module, Line) ->
    {Module,'',Line}.

highlight_line(#state{output=none}, _, _, _, _) ->
    ok;
highlight_line(#state{output=Dev}, dpapi, _, enter, Line) ->
    io:format(Dev, "~n~s~n~n~s~n", [lists:duplicate(80, $*), Line]);
highlight_line(#state{output=Dev}, _,_,_,Line) ->
    io:format(Dev, "~s~n", [Line]).

format_timestamp(Timestamp) ->
    {_,_,USec} = Timestamp,
    {{Yr,Mn,D},{Hr,Min,Sec}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0w-~2..0w-~2..0w::~2..0w:~2..0w:~2..0w.~3..0w",
                  [Yr, Mn, D, Hr, Min, Sec, USec div 1000]).
