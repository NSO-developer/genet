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

%%-define(LEVELMODE, cdb).
-define(LEVELMODE, file).

-define(CONFIGFILE, "loglevels.conf").
-define(DEFAULT_OUTPUT, "logs/ncs-genet.log").

-record(state, {output=none, levels=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    case get_config_file() of
        {_, {ok, Dev}} ->
            ConfState = get_config_state(Dev),
            file:close(Dev);
        {Filename, {error, Err}} ->
            eclog(error, "Cannot open file ~p for reading: ~p", [Filename, Err]),
            ConfState = #state{}
    end,
    eclog(info, "logger started", []),
    {ok, ConfState}.


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
process(Msg, State) ->
    eclog(error, "Got unexpected request: ~p", [Msg]),
    State.

eclog(error, Format, Args) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: " ++ Format, [?SERVER | Args]);
eclog(info, Format, Args) ->
    econfd:log(?CONFD_LEVEL_INFO,  "~p: " ++ Format, [?SERVER | Args]);
eclog(trace, Format, Args) ->
    econfd:log(?CONFD_LEVEL_TRACE, "~p: " ++ Format, [?SERVER | Args]).

%%%%%%%%%
%%% Config reading
%%%%%%%%%

%% @doc Lookup logger config file and open it for reading.
-spec get_config_file() -> {Filename::string(), {error, Reason::any()} | {ok, pid()}}.
get_config_file() ->
    Filename = case application:get_env(ec_genet, logconfig) of
                   undefined -> ?CONFIGFILE;
                   {ok, Fname} -> Fname
               end,
    {Filename, file:open(Filename, [read])}.

%% @doc Read and parse logger config and create #state record to be
%% used.  If logger config processing fails, return the default state
%% structure (i.e. no logging at all).
-spec get_config_state(pid()) -> #state{}.
get_config_state(Device) ->
    case read_config(Device) of
        {ok, Conf} ->
            {file, OutputName} = lists:keyfind(file, 1, Conf),
            {levels, Levels} = lists:keyfind(levels, 1, Conf),
            case file:open(OutputName, [write,delayed_write]) of
                {ok, Dev} ->
                    #state{output=Dev, levels=Levels};
                Err ->
                    eclog(error, "Failed to open file ~p for writing: ~p", [OutputName, Err]),
                    #state{}
            end;
        Err ->
            eclog(error, "Failed to parse logging config: ~p", [Err]),
            #state{}
    end.

%% @doc Read the logger configuration and parse it to the form of
%% tuple list.  The tuple list always contains `file` and `levels`
%% entries, at least with default value.
-spec read_config(pid()) -> {ok, Config::[tuple()]} | {error, Reason::string(), any()}.
read_config(Device) ->
    InitConfig = [{levels, []}, {file, ?DEFAULT_OUTPUT}],
    case io:scan_erl_exprs(Device, "") of
        {ok, Result, _Loc} ->
            read_config(Result, InitConfig);
        {eof, _} ->
            {ok, InitConfig}
    end.

-spec read_config([Token::tuple()], Config::[tuple()]) ->
                         {ok, Config::[tuple()]} | {error, Reason::string(), any()}.
read_config([{atom,Line,Key},{'=',Line}|Rest1], Config) ->
    {ConfigItem, Rest2} = case Key of
                              levels ->
                                  read_levels(Rest1, Line);
                              file ->
                                  read_file(Rest1, Line)
                          end,
    read_config(Rest2, [{Key, ConfigItem} | Config]);
read_config([], Config) ->
    {ok, Config};
read_config(Junk, _) ->
    {error, "junk at the end of config file", Junk}.

-spec read_levels([Token1::tuple()], integer()) -> {[Level::atom()], [Token2::tuple()]}.
read_levels([{',',Line}|Rest], Line) ->
    read_levels(Rest, Line);
read_levels([{atom,Line,Name}|Rest1], Line) ->
    {Levels,Rest2} = read_levels(Rest1, Line),
    {[Name|Levels],Rest2};
read_levels(Rest,_) ->
    {[], Rest}.

-spec read_file([Token1::tuple()], integer()) -> {Name::string(), [Token2::tuple()]}.
read_file([{string,Line,Name}|Rest], Line) ->
    {Name, Rest};
read_file(Tokens, Line) ->
    {Name, Rest} = read_file_tokens(Tokens, Line),
    {lists:flatten(Name), Rest}.

read_file_tokens([{C,Line}|Rest1], Line) ->
    {Name,Rest} = read_file_tokens(Rest1, Line),
    {[atom_to_list(C)|Name],Rest};
read_file_tokens([{integer,Line,Int}|Rest1], Line) ->
    {Name,Rest} = read_file_tokens(Rest1, Line),
    {[integer_to_list(Int)|Name],Rest};
read_file_tokens([{_Cat,Line,Atom}|Rest1], Line) ->
    {Name,Rest} = read_file_tokens(Rest1, Line),
    {[atom_to_list(Atom)|Name],Rest};
read_file_tokens(Rest, _) ->
    {[],Rest}.

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
