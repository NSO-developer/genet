-module(ec_genet_logger).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

-export([before_advice/1,after_advice/2,throw_advice/2,log/4]).

-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
-include("ec_genet.hrl").
-include("log_groups.hrl").

-define(SERVER, ?MODULE).

-record(state, {output=none, filename=none, levels=[], groups=dict:new()}).
-define(ALL_LEVELS, [debug, info, note, warn, error]).  % ordering is important

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    %% initial config read in the subscriber
    ConfState = #state{groups=process_log_groups()},
    eclog(info, "logger started", []),
    {ok, ConfState}.

process_log_groups() ->
    GroupDeepList = lists:map(fun process_log_group/1, ?LOG_LEVELS),
    dict:from_list(lists:flatten(GroupDeepList)).

process_log_group({Group, Level}) ->
    [process_log_group_module(Group, Level, Module, Funs) ||
        {Module, Funs} <- proplists:get_value(Group, ?LOG_GROUPS, [])].

process_log_group_module(Group, Level, Module, Funs) ->
    [{{Module, Fun}, {Group, Level}} || Fun <- Funs].

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

%%%%%%%%%
%%% Logging API functions
%%%%%%%%%

before_advice(Desc) ->
    request({before_advice, Desc}).

after_advice(Desc, R) ->
    request({after_advice, Desc, R}).

throw_advice(Desc, {Exc,R}) ->
    request({throw_advice, Desc, {Exc, R}}),
    erlang:raise(Exc, R, erlang:get_stacktrace()).

log(Level, Module, Line, Args) ->
    request({log, Level, Module, Line, Args}).

%%%%%%%%%
%%% Internal server request processing
%%%%%%%%%

request(Request) ->
    gen_server:cast(?SERVER, {Request, os:timestamp()}).

process({{before_advice, [M, Fn, Line, Args]}, Timestamp}, State) ->
    format_log_level(State, Timestamp, fix_location(M, Fn, Line), enter, [Args]),
    State;
process({{after_advice, [M, Fn, Line, _Args], R}, Timestamp}, State) ->
    format_log_level(State, Timestamp, fix_location(M, Fn, Line), exit, [R]),
    State;
process({{throw_advice, [M, Fn, Line, _Args], {Exc, R}}, Timestamp}, State) ->
    format_log_level(State, Timestamp, fix_location(M,Fn,Line), exception, [Exc, R]),
    State;
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
    case file:open(File, [append, delayed_write]) of
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

format_log_level(State, Timestamp, Location={Module,Function,_Line}, Event, Args) ->
    Level = lookup_level(State#state.groups, Module, Function, Event),
    output_log(State, Timestamp, Level, Event, Location, Args).

%% @doc For given function and event type, lookup log level.  For
%% event type `exception` it is always `warn`.
-spec lookup_level(Groups::dict(), Module::atom(), Function::atom(), Event::atom()) ->
                          {LogGroup::atom(), Level::atom()}.
lookup_level(Groups, Module, Function, exception) ->
    {Group,_Level} = lookup_level(Groups, Module, Function),
    {Group,warn};
lookup_level(Groups, Module, Function, _Event) ->
    lookup_level(Groups, Module, Function).

%% TODO: convert this to dict lookup
lookup_level(Groups, Module, Function) ->
    case dict:find({Module, Function}, Groups) of
        {ok, Value} -> Value;
        error -> {unknown, unknown}
    end.

%% @doc Format new log entry and write it to the output file.
-spec output_log(#state{},
                 os:timestamp(),
                 {Group::atom(),Level::atom()},
                 Event::atom(),
                 {Module::atom(), Function::atom(), Line::integer()},
                 Args::[term(),...]) ->
                        ok.
output_log(#state{levels=Levels, output=Dev}, Timestamp, {Group,Level}, Event, Location, Args) ->
    case lists:member(Level, Levels) of
        true ->
            do_output_log(Dev, Timestamp, Group, Level, Event, Location, Args);
        _ ->
            ok
    end.

do_output_log(Dev, Timestamp, Group, Level, Event, Location, Args) ->
    LogList = case Event of
                  exception -> [R,Exc] = Args, throw_log(R, Exc);
                  enter -> [Arg] = Args, enter_log(Arg);
                  exit -> [Ret] = Args, exit_log(Ret);
                  _ -> [Format, Arg] = Args, io_lib:format(Format, Arg)
              end,
    LogEntry = lists:flatten(LogList),
    highlight_line(Dev, Group, Level, Event,
                   io_lib:format("~s [~p;~p] ~s ~s",
                                 [format_timestamp(Timestamp), Level, Group, format_location(Location), LogEntry])).

format_location({Module, '', Line}) ->
    io_lib:format("~p:~p", [Module, Line]);
format_location({Module, Function, Line}) ->
    io_lib:format("~p:~p:~p", [Module, Line, Function]).

location(Module, Line) ->
    {Module,'',Line}.

highlight_line(none, _, _, _, _) ->
    ok;
highlight_line(Dev, dpapi, _, enter, Line) ->
    io:format(Dev, "~n~s~n~n~s~n", [lists:duplicate(80, $*), Line]);
highlight_line(Dev, _,_,_,Line) ->
    io:format(Dev, "~s~n", [Line]).

format_timestamp(Timestamp) ->
    {_,_,USec} = Timestamp,
    {{Yr,Mn,D},{Hr,Min,Sec}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0w-~2..0w-~2..0w::~2..0w:~2..0w:~2..0w.~3..0w",
                  [Yr, Mn, D, Hr, Min, Sec, USec div 1000]).

fix_location(Module, Fn, Line) ->
    {Module,fix_funcname(Fn),Line}.

fix_funcname(Fn) ->
    Fnl = atom_to_list(Fn),
    list_to_atom(lists:sublist(Fnl, length(Fnl)-2)).

enter_log(Args) ->
    case lists:map(fun format_arg/1, Args) of
        [] ->
            "{enter, []}";
        [Af|Afs] ->
            io_lib:format("{enter, [~s~s]}", [Af, lists:map(fun (Arg) -> ", " ++ Arg end, Afs)])
    end.

exit_log(RetVal) ->
    io_lib:format("{exit, ~s}", [format_arg(RetVal)]).

throw_log(R, Exc) ->
    io_lib:format("{exception, {~p,~p}}", [R, Exc]).

format_arg(A) ->
    format_val(A, 0).

-define(MAX_DEPTH, 7).

%% @doc Format a value for logging.  Recurses into tuples, lists, and
%% (some) structures up to given recursion level.  Note that the
%% return value may actually be a deep list.
-spec format_val(Value::term(), Depth::integer()) -> string().
format_val(#confd_trans_ctx{},_) ->
    "tctx";
format_val(#maapi_cursor{},_) ->
    "cursor";
format_val(#mappings{}, ?MAX_DEPTH) ->
    "#mappings{...}";
format_val(Map=#mappings{},L) ->
    Fields = lists:zip(record_info(fields, mappings), tl(tuple_to_list(Map))),
    FormFields = [format_mapfield(F,L) || F <- Fields],
    io_lib:format("#mappings{~s}", [string:join(lists:filter(fun (Str) -> Str /= "" end, FormFields),
                                                ", ")]);
format_val(T, ?MAX_DEPTH) when is_list(T) ->
    "[...]";
format_val([A|B], L) when not is_list(B) ->
    io_lib:format("[~s|~s]", [format_val(A, L+1), format_val(B, L+1)]);
format_val([], _) ->
    "[]";
format_val(Vals, L) when is_list(Vals) ->
    case io_lib:printable_list(Vals) of
        true ->
            Vals;
        _ ->
            case catch length(Vals) of
                {'EXIT', _} ->
                    "[" ++ io_lib:format("[~s|~s]",
                                         [format_val(hd(Vals), L+1),
                                          format_val(tl(Vals), L+1)]);
                _ ->
                    io_lib:format("[~s]", [string:join([format_val(V, L+1) || V <- Vals], ", ")])
            end
    end;
format_val(T, ?MAX_DEPTH) when is_tuple(T) ->
    "{...}";
format_val(Vals, L) when is_tuple(Vals) ->
    io_lib:format("{~s}", [string:join([format_val(V, L+1) || V <- tuple_to_list(Vals)], ", ")]);
format_val(Arg,_) ->
    %% FIXME: this may yield too long expressions and break lines
    io_lib:format("~p", [Arg]).

format_mapfield({_,none},_) ->
    "";
format_mapfield({Name,Value},L) ->
    io_lib:format("~p=~s", [Name, format_val(Value, L+1)]).
