-module(ec_genet_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         reg_mapping/3, unreg_mapping/2,
         tctx_maapi_sock/1,tctx_maapi_thandle/1,convert_value/2, convert_path/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
-include("ec_genet.hrl").
-include("debug_macros.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true), % Triggers call to terminate/2
    TransDP = #confd_trans_cbs{
      init = fun s_init/1,
      finish = fun s_finish/1},
    DP = #confd_data_cbs{
      get_elem        = fun get_elem/2,
      exists_optional = fun exists/2,
      get_next        = fun get_next/3,
      set_elem        = fun set_elem/3,
      get_case        = fun get_case/3,
      set_case        = fun set_case/4,
      create          = fun create/2,
      remove          = fun delete/2,
      move_after      = fun move_after/3,
      callpoint       = ec_genet},

    Port = application:get_env(ec_genet, port, ?NCS_PORT),
    {ok, M} = econfd_maapi:connect({127,0,0,1}, Port),
    {ok, Daemon} = econfd:init_daemon(transform, ?GENET_TRACE_LEVEL, user,
                                      M, {127,0,0,1}, Port),
    ok = econfd:register_trans_cb(Daemon, TransDP),
    ok = econfd:register_data_cb(Daemon, DP),
    ok = econfd:register_done(Daemon),
    ets:new(ec_genet_maps, [ordered_set, public, named_table, {read_concurrency,true}]),
    log(info, "Server started", []),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% handle_call(ping, _From, State) ->
%%     Reply = pong,
%%     {reply, Reply, State};
handle_call(Req, _From, State) ->
    log(error, "Got unexpected call: ~p", [Req]),
    Reply = error,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    log(error, "Got unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(Info, State) ->
    log(error, "Got unexpected info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    log(info, "Server stopped - ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log(error, Format, Args) ->
    econfd:log(?CONFD_LEVEL_ERROR, "~p: " ++ Format, [?SERVER | Args]);
log(info, Format, Args) ->
    econfd:log(?CONFD_LEVEL_INFO,  "~p: " ++ Format, [?SERVER | Args]);
log(trace, Format, Args) ->
    econfd:log(?CONFD_LEVEL_TRACE, "~p: " ++ Format, [?SERVER | Args]).

%%%===================================================================
%%% Registration Calls
%%%===================================================================

reg_mapping(Prio, Path, MapFun) ->
    true = ets:insert(ec_genet_maps,{{-Prio,lists:reverse(Path)},MapFun}),
    log(info, "ec_genet_server registered prio ~p path prefix~n~p", [Prio,Path]),
    ok.

unreg_mapping(Prio, Path) ->
    true = ets:delete(ec_genet_maps,{-Prio,lists:reverse(Path)}),
    log(info, "ec_genet_server unregistered prio ~p path prefix~n~p",[Prio,Path]),
    ok.

%%%===================================================================
%%% Mapping Multiplexing
%%%===================================================================

get_mappings(HLPath) ->
    MappingFun =
        case get_mapping_fun(get_module_prefix(HLPath),ets:first(ec_genet_maps)) of
            none ->
                ?LOGWARN("Lookup of HLPath does not match any registered module, using null map", HLPath),
                fun(_) -> none end;
            NormalMappingFun ->
                ?LOGINFO("Mapping module used", NormalMappingFun),
                NormalMappingFun
        end,
    try
        get_mapping_record(MappingFun,HLPath)
    catch
        Reason:Error ->
            Trace = erlang:get_stacktrace(),
            ?LOGWARN("Mapping record failure, using null map", HLPath, Reason, Error, Trace),
            get_mapping_record(fun(_) -> none end,HLPath)
    end.

get_mapping_record(MappingFun,HLPath) ->
    InputRecord = MappingFun(HLPath),
    case InputRecord of
        none ->
            %% Default mapping if no corresponding module found
            ?LOGWARN("Lookup of HLPath does not match any registered element, using null map", HLPath),
            #mappings{get_elem=fun(_,_,_) -> not_found end,
                      exists  =fun(_,_,_) -> not_found end,
                      get_next=fun(_,_,_,_) -> {false,undefined} end,
                      get_case=fun(_,_,_,_) -> not_found end,
                      set_elem=fun(_,_,_,_) -> ok end,
                      create  =fun(_,_,_) -> ok end,
                      delete  =fun(_,_,_) -> ok end,
                      set_case=fun(_,_,_,_,_) -> ok end,
                      move_after=fun(_,_,_,_) -> ok end
                     };
        #mappings{inherit=InheritedPath} when InheritedPath /= none ->
            %% We should inherit the mapping record from the given path
            InheritedRecord = get_mapping_record(MappingFun,InheritedPath),
            %% Any mappings provided by the child prevail
            MergedRecord = ec_genet_mapgens:merge_mappings(InputRecord, InheritedRecord),
            %% If the child specified a relpath, compute the end result
            MergedPath = MergedRecord#mappings.path,
            NewPath =
                case InputRecord#mappings.relpath of
                    none -> MergedPath;
                    RelPath -> RelPath ++ MergedPath
                end,
            %% ... and patch the result with the new path
            MergedRecord#mappings{path=NewPath};
        _ ->
            InputRecord
    end.

get_module_prefix(HLPath) ->
    lists:map(fun(X) when is_tuple(X) -> {};
                 (X) -> X end,
              lists:reverse(HLPath)).

get_mapping_fun(_RevHLPath, '$end_of_table') ->
    none;
get_mapping_fun(RevHLPath, Key = {Prio, RevPrefixPath}) ->
    case lists:prefix(RevPrefixPath, RevHLPath) of
        true ->
            [{_, MappingFun}] = ets:lookup(ec_genet_maps, Key),
            MappingFun;
        false ->
            get_mapping_fun(RevHLPath, ets:next(ec_genet_maps,{Prio, RevPrefixPath}))
    end.


%%%===================================================================
%%% Trans Callbacks
%%%===================================================================

s_init(Tctx) ->
    M = (Tctx#confd_trans_ctx.dx)#confd_daemon_ctx.d_opaque,
    econfd_maapi:attach(M, 0, Tctx),
    %% TH = Tctx#confd_trans_ctx.thandle,
    %% LogLevel = get_current_loglevel(M, TH),
    %% inform_about_loglevel(Tctx, LogLevel),
    ?LOGNOTE("========== Transaction initialized =========="),
    {ok, Tctx#confd_trans_ctx{opaque = M}}.

s_finish(Tctx) ->
    M = tctx_maapi_sock(Tctx),
    TH = Tctx#confd_trans_ctx.thandle,
    try
        ok = econfd_maapi:detach(M, TH)
    catch
        _:Error ->
            ?LOGERR("Could not detach from transaction", Error)
    end,
    ?LOGNOTE("========== Transaction finished =========="),
    ok.

%%%===================================================================
%%% Data Callbacks
%%%===================================================================

get_elem(Tctx, HLPath) ->
    Mappings = get_mappings(HLPath),
    case process_mapping(get_elem, Tctx, HLPath, none, Mappings) of
        NFVal={ok,not_found} ->
            NFVal;
        {ok,Val} ->
            {ok,convert_value(HLPath, Val)};
        V -> V
    end.
exists(Tctx, HLPath) ->
    Mappings = get_mappings(HLPath),
    process_mapping(exists, Tctx, HLPath, none, Mappings).
set_elem(Tctx, HLPath, HLVal) ->
    Mappings = get_mappings(HLPath),
    process_mapping(set_elem, Tctx, HLPath, HLVal, Mappings).
create(Tctx, HLPath) ->
    Mappings = get_mappings(HLPath),
    process_mapping(create, Tctx, HLPath, none, Mappings).
delete(Tctx, HLPath) ->
    Mappings = get_mappings(HLPath),
    process_mapping(delete, Tctx, HLPath, none, Mappings).
move_after(Tctx, HLPath, PrevKeys) ->
    Mappings = get_mappings(HLPath),
    process_mapping(move_after, Tctx, HLPath, PrevKeys, Mappings).
get_next(Tctx, HLPath, Next) ->
    Mappings = get_mappings(HLPath),
    case process_mapping(get_next, Tctx, HLPath, Next, Mappings) of
        RV={ok,{false,_}} ->
            RV;
        {ok,{Keys,C}} ->
            {ok,{convert_key_values(HLPath, Keys),C}};
        V -> V
    end.

get_case(Tctx, HLPath, Choice) ->
    Mappings = get_mappings([Choice|HLPath]),
    process_mapping(get_case, Tctx, HLPath, Choice, Mappings).
set_case(Tctx, HLPath, Choice, Case) ->
    Mappings = get_mappings([Choice|HLPath]),
    process_mapping(set_case, Tctx, HLPath, {Choice, Case}, Mappings).

%%%===================================================================
%%% Mapping Function Interface
%%%===================================================================

throw_callmap_error(CallmapError,CallmapReason,Name) ->
    Trace = erlang:get_stacktrace(),
    FileLine =
        case Trace of
            [{_,_,_,[{file,FName},{line,FLine}]}|_] ->

                " at "++FName++":"++integer_to_list(FLine);
            _ ->
                ""
        end,
    ?LOGERR("Caught exception in "++Name++" mapping function"++FileLine, CallmapError, CallmapReason, Trace),
    throw({CallmapError, CallmapReason}).

-define(CALLMAP(Name, RetMatch, Fun, Tctx, Arg1, Arg2),
        try
            ?LOGINFO("Calling "++??Name++" mapping function", tctx, Arg1, Arg2),
            RetMatch = Fun(Tctx, Arg1, Arg2),
            ?LOGINFO("Returned from "++??Name++" mapping function", RetMatch),
            {ok, RetMatch}
        catch
            CallmapError:CallmapReason ->
                throw_callmap_error(CallmapError,CallmapReason,??Name)
        end).

-define(CALLMAP(Name, RetMatch, Fun, Tctx, Arg1, Arg2, Arg3),
        try
            ?LOGINFO("Calling "++??Name++" mapping function", tctx, Arg1, Arg2, Arg3),
            RetMatch = Fun(Tctx, Arg1, Arg2, Arg3),
            ?LOGINFO("Returned from "++??Name++" mapping function", RetMatch),
            {ok, RetMatch}
        catch
            CallmapError:CallmapReason ->
                throw_callmap_error(CallmapError,CallmapReason,??Name)
        end).

-define(CALLMAP(Name, RetMatch, Fun, Tctx, Arg1, Arg2, Arg3, Arg4),
        try
            ?LOGINFO("Calling "++??Name++" mapping function", tctx, Arg1, Arg2, Arg3, Arg4),
            RetMatch = Fun(Tctx, Arg1, Arg2, Arg3, Arg4),
            ?LOGINFO("Returned from "++??Name++" mapping function", RetMatch),
            {ok, RetMatch}
        catch
            CallmapError:CallmapReason ->
                throw_callmap_error(CallmapError,CallmapReason,??Name)
        end).

-define(CALLMAP(Name, RetMatch, Fun, Tctx, Arg1, Arg2, Arg3, Arg4, Arg5),
        try
            ?LOGINFO("Calling "++??Name++" mapping function", tctx, Arg1, Arg2, Arg3, Arg4, Arg5),
            RetMatch = Fun(Tctx, Arg1, Arg2, Arg3, Arg4, Arg5),
            ?LOGINFO("Returned from "++??Name++" mapping function", RetMatch),
            {ok, RetMatch}
        catch
            CallmapError:CallmapReason ->
                throw_callmap_error(CallmapError,CallmapReason,??Name)
        end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_mapping(Op, Tctx, HLPath, Arg, Mappings) ->
    try
        Res = process_mapping_fexists(Op, Tctx, HLPath, Arg, Mappings),
        case Res of
            ok           -> ok;
            {error, Err} -> throw({error, Err});
            RetVal       -> {ok, RetVal}
        end
    catch
        _:{error, Msg} ->
            {error, format_error(Msg)};
        _:{throw,{error, Msg}} ->
            {error, format_error(Msg)};
        Error:Reason ->
            Trace = erlang:get_stacktrace(),
            FileLine =
                case Trace of
                    [{_,_,_,[{file,FName},{line,FLine}]}|_] ->

                        " at "++FName++":"++integer_to_list(FLine);
                    _ ->
                        ""
                end,
            ?LOGERR("Caught exception"++FileLine, HLPath, Error, Reason, Trace),
            {error, lists:flatten(io_lib:format("transformation failure, ~p", [Error]))}
    end.

process_mapping_fexists(Op, Tctx, HLPath, Arg, Mappings) ->
    Extra = Mappings#mappings.extra,
    %% If fexists is defined, call that first to decide
    %% whether there is any point in calling anything else
    case {Op, Mappings} of
        {get_elem, #mappings{fexists=Fexists}} when Fexists /= none ->
            Res = ?CALLMAP(fexists,Val,Fexists,Tctx,HLPath, Extra),
            case Res of
                {ok, not_found} -> not_found;
                _ -> process_mapping_override(Op, Tctx, HLPath, Arg, Mappings)
            end;
        {exists, #mappings{fexists=Fexists}} when Fexists /= none ->
            Res = ?CALLMAP(fexists,Existence,Fexists,Tctx,HLPath, Extra),
            case Res of
                {ok, not_found} -> not_found;
                _ -> process_mapping_override(Op, Tctx, HLPath, Arg, Mappings)
            end;
        {get_next, #mappings{fexists=Fexists}} when Fexists /= none ->
            Res = ?CALLMAP(fexists,NotFoundOrNextKeyAndCursor,Fexists,Tctx,HLPath, Extra),
            case Res of
                {ok, not_found} -> {false, undefined};
                _ -> process_mapping_override(Op, Tctx, HLPath, Arg, Mappings)
            end;
        {get_case, #mappings{fexists=Fexists}} when Fexists /= none ->
            Res = ?CALLMAP(fexists,Case,Fexists,Tctx,HLPath, Extra),
            case Res of
                {ok, not_found} -> not_found;
                _ -> process_mapping_override(Op, Tctx, HLPath, Arg, Mappings)
            end;
        {delete, #mappings{fexists=Fexists}} when Fexists /= none ->
            Res = ?CALLMAP(fexists,Deleted,Fexists,Tctx,HLPath, Extra),
            case Res of
                {ok, not_found} -> ok;
                _ -> process_mapping_override(Op, Tctx, HLPath, Arg, Mappings)
            end;
        _ -> process_mapping_override(Op, Tctx, HLPath, Arg, Mappings)
    end.

process_mapping_override(Op, Tctx, HLPath, Arg, Mappings) ->
    Extra = Mappings#mappings.extra,
    case {Op, Mappings} of
        %% Custom override implementations of DP callback functions
        {get_elem, #mappings{get_elem=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(get_elem,Val,Fcb,Tctx,HLPath,Extra),
            Ret;
        {exists, #mappings{exists=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(exists,Existence,Fcb,Tctx,HLPath,Extra),
            Ret;
        {exists, #mappings{get_elem=Fcb}} when Fcb /= none ->
            Res = ?CALLMAP(get_elem,Existence,Fcb,Tctx,HLPath,Extra),
            case Res of
                {ok, not_found} -> not_found;
                {ok, _} -> true;
                Error ->
                    ?LOGERR("Caught exception in exists default call to get_elem mapping function", HLPath, Error),
                    not_found
            end;
        {create, #mappings{create=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(create,ok,Fcb,Tctx,HLPath,Extra),
            Ret;
        {delete, #mappings{delete=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(delete,ok,Fcb,Tctx,HLPath,Extra),
            Ret;
        {move_after, #mappings{move_after=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(move_after, ok, Fcb, Tctx, HLPath, Arg, Extra),
            Ret;
        {set_elem, #mappings{set_elem=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(set_elem,ok,Fcb,Tctx,HLPath,Arg,Extra),
            Ret;
        {get_next, #mappings{get_next=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(get_next,{NextKeySet,NextCursor},Fcb,Tctx,HLPath,Arg,Extra),
            Ret;
        {get_case, #mappings{get_case=Fcb}} when Fcb /= none ->
            {ok, Ret} = ?CALLMAP(get_case,Case,Fcb,Tctx,HLPath,Arg,Extra),
            Ret;
        {set_case, #mappings{set_case=Fcb}} when Fcb /= none ->
            {Choice, Case} = Arg,
            {ok, Ret} = ?CALLMAP(set_case,ok,Fcb,Tctx,HLPath,Choice,Case,Extra),
            Ret;

        %% FIXME: implement additional functions; *_object/...

        _ ->
            process_mapping_core(Op, Tctx, HLPath, Arg, Mappings)
    end.

% Normal case: no custom get/set/... implementation
process_mapping_core(Op, Tctx, HLPath, Arg, Mappings) ->
    case Op of
        get_elem ->
            %% Arg is not used
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, Arg, Mappings) of
                {ok, RawVal} ->
                    process_value(Op, Tctx, RawVal, Mappings, HLPath);
                not_found ->
                    not_found;
                {error,Error} ->
                    {error,Error}
            end;
        exists ->
            %% Arg is not used
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, Arg, Mappings) of
                {ok, RawVal} ->
                    process_value(Op, Tctx, RawVal, Mappings, HLPath);
                not_found ->
                    not_found;
                {error,Error} ->
                    {error,Error}
            end;
        create ->
            %% Arg is not used
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, Arg, Mappings) of
                {ok, _} -> ok;
                ok      -> ok;
                {error, Error} -> {error, Error}
            end;
        delete ->
            %% Arg is not used
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, Arg, Mappings) of
                {ok, _} -> ok;
                ok      -> ok;
                {error, Error} -> {error, Error}
            end;
        set_elem ->
            %% Arg is the value to set
            LLVal = process_value(Op, Tctx, Arg, Mappings, HLPath),
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, LLVal, Mappings) of
                {ok, _} -> ok;
                ok      -> ok;
                {error, Error} -> {error, Error}
            end;
        get_next ->
            %% Arg is the cursor
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, Arg, Mappings) of
                {ok, RawVal} ->
                    %% FIXME: call to aggregator
                    process_value(Op, Tctx, RawVal, Mappings, HLPath);
                [{ok, RawVal}] ->
                    process_value(Op, Tctx, RawVal, Mappings, HLPath);
                {error,Error} ->
                    ?LOGERR("Retuning error",HLPath,Error),
                    {error,Error}
            end;
        get_case ->
            %% Arg is the Choice id
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, Arg, Mappings) of
                {ok, RawVal} ->
                    process_value(Op, Tctx, RawVal, Mappings, HLPath);
                not_found ->
                    not_found;
                {error,Error} ->
                    {error,Error}
            end;
        set_case ->
            %% Arg is the value {Choice, Case}
            LLVal = process_value(Op, Tctx, Arg, Mappings, HLPath),
            Path = process_path(Tctx, HLPath, Mappings),
            case process_opmap(Op, Tctx, Path, LLVal, Mappings) of
                {ok, _} -> ok;
                ok      -> ok;
                {error, Error} -> {error, Error}
            end;

        %% FIXME: Implement more operation cases, *_object/...

        _ ->
            {error, not_implemented}
    end.

process_opmap(Op, Tctx, LLPath, Arg, Mappings) ->
    Path = path_rewrite(Op, Tctx, LLPath, Arg, Mappings),
    case Mappings of
        #mappings{fopmap=FopMap} when FopMap /= none ->
            {ok, {RetTctx, RetOp, RetPath, RetArg, RetMappings}} =
                ?CALLMAP(fopmap,{NewTctx, NewOp, NewPath, NewArg, NewMappings},FopMap,Tctx,Op,Path,Arg,Mappings),
            process_nested(RetOp, RetTctx, RetPath, RetArg, RetMappings);
        _ ->
            process_nested(Op, Tctx, Path, Arg, Mappings)
    end.

process_nested(Op, Tctx, LLPath, NestingArg, Mappings) ->
    case Mappings of
        #mappings{nested=NestedMappings} when NestedMappings /= none, is_list(NestingArg) ->
            ?LOGMSG("Nesting multiple",LLPath, NestingArg),
            NestingResult =
                {ok, lists:map(
                       fun({NestedMapping,NthArg}) ->
                               case process_mapping(Op, Tctx, LLPath, NthArg, NestedMapping) of
                                   {ok, Val} -> Val;
                                   ok -> ok;
                                   Error={error, _} -> Error
                               end
                       end,
                       lists:zip(NestedMappings,NestingArg))},
            ?LOGMSG("Nesting multiple done",NestingResult),
            NestingResult;
        #mappings{nested=NestedMappings} when NestedMappings /= none ->
            ?LOGMSG("Nesting single",LLPath, NestingArg),
            NestingResultSingle =
                {ok, lists:map(
                       fun(NestedMapping) ->
                               case process_mapping(Op, Tctx, LLPath, NestingArg, NestedMapping) of
                                   {ok, Val} -> Val;
                                   ok -> ok;
                                   Error={error, _} -> Error
                               end
                       end,
                       NestedMappings)},
            ?LOGMSG("Nesting single done",NestingResultSingle),
            NestingResultSingle;
        _ -> % No nested mappings, so let's call the default implementations
            default_ll_op(Op, Tctx, LLPath, NestingArg, Mappings)
    end.

path_rewrite(_Op, Tctx, KeyedLLPath, _Arg, Mappings) ->
    case Mappings of
        #mappings{fdnpath=FdnPath} when FdnPath /= none ->
            Extra = Mappings#mappings.extra,
            {ok, NewLLPath} =
                ?CALLMAP(fdnpath,NewPath,FdnPath,Tctx,KeyedLLPath,Extra),
            NewLLPath;
        _ ->
            KeyedLLPath
    end.

process_path(Tctx, HLPath, Mappings) ->
    KeySet = extract_keys(HLPath),
    Extra = Mappings#mappings.extra,
    case Mappings of
        #mappings{path=RawLLPath, fdnkeys=FdnKeys} when FdnKeys /= none ->
            %% FIXME: Handle Case as well
            %%FdnKeys(Op, Tctx, HLPath, HLPath, Mappings);
            {ok, RetKeySet} = ?CALLMAP(fdnkeys,NewKeySet,FdnKeys,Tctx,KeySet,Extra),
            inject_keys(RawLLPath, RetKeySet);
        #mappings{path=RawLLPath} when RawLLPath /= none ->
            RawLLPath;
        #mappings{nested=Nested} when Nested /= none ->
            %% Ok, no path specified and no key functions
            %% but it has some nesting, so I guess they'll handle it
            %%?LOGMSG("Mapping element onto itself, but I guess nested mapping functions solve this", HLPath),
            HLPath;
        _ ->
            %%not_found
            {error, cannot_map_to_itself}
    end.

process_value(Op, Tctx, RawVal, Mappings, HLPath) ->
    Extra = Mappings#mappings.extra,
    case {Op, Mappings} of
        {get_elem, #mappings{fupval=FupVal}} when FupVal /= none ->
            {ok, RetVal} = ?CALLMAP(fupval,NewVal,FupVal,Tctx,Op,RawVal,Extra),
            RetVal;
        {exists, #mappings{fupval=FupVal}} when FupVal /= none ->
            {ok, RetVal} = ?CALLMAP(fupval,NewVal,FupVal,Tctx,Op,RawVal,Extra),
            RetVal;
        {create, _} ->
            RawVal;
        {delete, _} ->
            RawVal;
        {set_elem, #mappings{fdnval=FdnVal}} when FdnVal /= none ->
            {ok, RetVal} = ?CALLMAP(fdnval,NewVal,FdnVal,Tctx,Op,RawVal,Extra),
            %% Check that mapping function returned same number of
            %% values as there are nested mappings
            case {RetVal, Mappings} of
                {_, #mappings{nested=Nested}} when Nested == none ->
                    ok;
                {RetVal, #mappings{nested=Nested}} when not is_list(RetVal), Nested /= none ->
                    ExpectedLength = length(Nested),
                    ?LOGERR("Bad return value from mapping function, expected list",
                            HLPath, ExpectedLength, RetVal);
                {RetVal, #mappings{nested=Nested}} when is_list(RetVal), Nested /= none,
                                                        length(RetVal) /= length(Nested) ->
                    ExpectedLength = length(Nested),
                    ActualLength = length(RetVal),
                    ?LOGERR("Bad return value from mapping function, incorrect number of list elements",
                            HLPath, ExpectedLength, ActualLength);
                _ ->
                    ok
            end,
            RetVal;
        {get_next, #mappings{fupkeys=FupKeys}} when FupKeys /= none ->
            case RawVal of
                {Keys, C} ->
                    {ok, RetKeys} = ?CALLMAP(fupkeys,NewVal,FupKeys,Tctx,Op,Keys,Extra),
                    {RetKeys, C};
                NestedList when is_list(NestedList) ->
                    {ok, Ret} = ?CALLMAP(fupkeys,{RetKeys,RetState},FupKeys,Tctx,Op,NestedList,Extra),
                    Ret;
                _ -> RawVal
            end;
        {get_case, #mappings{fupval=FupVal}} when FupVal /= none ->
            {ok, RetVal} = ?CALLMAP(fupval,NewVal,FupVal,Tctx,Op,RawVal,Extra),
            RetVal;
        {set_case, #mappings{fdnval=FdnVal}} when FdnVal /= none ->
            {ok, RetVal} = ?CALLMAP(fdnval,NewVal,FdnVal,Tctx,Op,RawVal,Extra),
            RetVal;

        %% FIXME: Implement more operation cases, *_object/...

        _ ->
            ?LOGMSG("No value override", RawVal),
            RawVal
    end.

%%%===================================================================
%%% Default low level functions
%%%===================================================================

default_ll_op(get_next, Tctx, Path, -1, Mappings) ->
    %% Cursor value -1 indicates start from the beginning of the list
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    Cursor = econfd_maapi:init_cursor(M, TH, convert_path(Path)),
    default_ll_op(get_next, Tctx, Path, Cursor, Mappings);
default_ll_op(get_next, _Tctx, _Path, Cursor, _Mappings) ->
    %% Cursor value /= -1 indicates start from the previous position
    Next = econfd_maapi:get_next(Cursor),
    RawVal =
        case Next of
            done ->
                {ok, {false, undefined}};
            {ok, Keys, NewCursor} ->
                {ok, {Keys, NewCursor}}
        end,
    RawVal;
default_ll_op(find_next, _Tctx, _Path, {Cursor, Type, Start}, _Mappings) ->
    Next = econfd_maapi:find_next(Cursor, Type, Start),
    RawVal =
        case Next of
            done ->
                {ok, {false, undefined}};
            {ok, Keys, NewCursor} ->
                {ok, {Keys, {NewCursor, Type, Keys}}}
        end,
    RawVal;
default_ll_op(get_elem, Tctx, Path, none, _Mappings) ->
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    case econfd_maapi:get_elem(M, TH, convert_path(Path)) of
        OkVal={ok, _} ->
            OkVal;
        {error, ErrMsg} ->
            ErrorMessage = format_error(ErrMsg),
            ?LOGMSG("maapi:get_elem() returned an error; ignored, returning not_found",
                    Path, ErrorMessage),
            not_found
    end;
default_ll_op(exists, Tctx, Path, none, _Mappings) ->
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    case econfd_maapi:exists(M, TH, convert_path(Path)) of
        OkVal={ok, _} ->
            OkVal;
        {error, ErrMsg} ->
            ErrorMessage = format_error(ErrMsg),
            ?LOGMSG("maapi:exists() returned an error; ignored, returning not_found",
                    Path, ErrorMessage),
            not_found
    end;
default_ll_op(create, Tctx, Path, _Val, _Mappings) ->
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    case econfd_maapi:create(M, TH, convert_path(Path)) of
        ok -> ok;
        {error, {2, _}} ->
            ?LOGMSG("Creating already existing element, ignored", Path),
            ok
    end;
default_ll_op(delete, Tctx, Path, _Val, _Mappings) ->
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    case econfd_maapi:delete(M, TH, convert_path(Path)) of
        ok -> ok;
        {error, {1, _}} ->
            ?LOGMSG("Deleting non-existent element, ignored", Path),
            ok
    end;
default_ll_op(set_elem, Tctx, Path, Val, _Mappings) ->
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    CVal = convert_value(Path, Val),
    ok = econfd_maapi:set_elem(M, TH, convert_path(Path), CVal);
default_ll_op(get_case, Tctx, [LLChoice|Path], _HLChoice, _Mappings) ->
    M = tctx_maapi_sock(Tctx),
    TH = tctx_maapi_thandle(Tctx),
    case econfd_maapi:get_case(M, TH, convert_path(Path), LLChoice) of
        OkVal={ok, _} ->
            OkVal;
        {error, {1, _Msg}} ->
            not_found;
        {error, ErrMsg} ->
            ErrorMessage = format_error(ErrMsg),
            ?LOGMSG("maapi:get_case() returned an error; ignored, returning not_found",
                    Path, ErrorMessage),
            not_found
    end;
default_ll_op(set_case, _Tctx, _Path, {_Choice, _Case}, _Mappings) ->
    %% By default, nothing needs to be done for set_case
    ok;
default_ll_op(nop, _Tctx, _Path, _Arg, _Mappings) ->
    %% No operation; only possibly used by fopmap, need to return value compatible with
    %% other operations
    {ok,ok}.

%%%===================================================================
%%% Helper functions
%%%===================================================================

format_error(Term) ->
    lists:flatten(io_lib:format("~p",[Term])).
extract_keys(Path) ->
    lists:filter(fun(E) -> is_tuple(E) end, Path).

inject_keys(Path, KeySet) ->
    {InjectedPath, []} = lists:mapfoldl(fun inject_keys_int/2, KeySet, Path),
    InjectedPath.
inject_keys_int({}, [FirstKey|RemainingKeySet]) ->
    {FirstKey, RemainingKeySet};
inject_keys_int(E, KeySet) ->
    {E, KeySet}.

tctx_maapi_sock(Tctx) ->
    Tctx#confd_trans_ctx.opaque.
tctx_maapi_thandle(Tctx) ->
    Tctx#confd_trans_ctx.thandle.

%% @spec convert_value(Path, value()) -> value()
%%
%% @doc Converts the value type such that it corresponds to the type expected for the
%% path.  Can convert between integer types, from string to ConfD string representations,
%% and from atom or string to enum.
convert_value(Path, Val) ->
    CVal = do_convert_value(Path, Val),
    if CVal /= Val ->
            ?LOGMSG("Converting values before passing to ConfD", Path, Val, CVal);
       true -> ok
    end,
    CVal.

check_convertible(Cs) ->
    case Cs#confd_cs_node.primitive_type of
        PType when PType >= ?C_INT8, PType =< ?C_UINT64 ->
            int;
        ?C_BUF ->
            buf;
        ?C_ENUM_VALUE ->
            enum;
        ?C_LIST ->
            %% not converting leaf-lists (yet)
            false;
        _ ->
            other
    end.

value_type({Type, _}) ->
    Type;
value_type(Bool) when Bool == false; Bool == true ->
    ?C_BOOL;
value_type(Bin) when is_binary(Bin) ->
    ?C_BUF;
value_type(Int) when is_integer(Int) ->
    ?C_INT32;
value_type(Double) when is_float(Double) ->
    ?C_DOUBLE;
value_type(Tuple) when is_tuple(Tuple), tuple_size(Tuple) == 4 ->
    ?C_IPV4;
value_type(Tuple) when is_tuple(Tuple), tuple_size(Tuple) == 8 ->
    ?C_IPV6;
value_type(List) when is_list(List) ->
    ?C_LIST; % could actually be also a string
value_type(_) ->
    ?C_NOEXISTS.

do_convert_value(Path, Val) ->
    Cs = econfd_schema:ikeypath2cs(Path),
    ValType = value_type(Val),
    case Cs of
        not_found ->
            Val;
        #confd_cs_node{primitive_type = ValType} ->
            %% no conversion needed
            Val;
        _ ->
            case check_convertible(Cs) of
                int -> convert_int_value(Cs, Val, Path);
                buf -> convert_other(Val, ValType, Cs);
                enum -> convert_enum_value(Val,Cs);
                other -> convert_other(Val, ValType, Cs);
                _ -> Val
            end
    end.

convert_other(Val, ValType, Cs) ->
    case value_to_string(Val, ValType) of
        {ok, Str} ->
            case econfd_schema:str2val(Cs, Str) of
                {ok, CVal} -> CVal;
                _ -> Val
            end;
        _ ->
            Val
    end.

value_to_string(Int, ?C_INT32) ->
    {ok, integer_to_list(Int)};
value_to_string({Type, Int}, Type) when Type >= ?C_INT8, Type =< ?C_UINT64 ->
    {ok, integer_to_list(Int)};
value_to_string(Bin, ?C_BUF) ->
    {ok, Bin};
value_to_string(String, ?C_LIST) ->
    {ok, list_to_binary(String)};
value_to_string(Val, Type) ->
    TypePair = case Type of
                   ?C_DOUBLE ->
                       {'http://www.w3.org/2001/XMLSchema', 'double'};
                   ?C_IPV4 ->
                       {'urn:ietf:params:xml:ns:yang:ietf-inet-types', 'ipv4-address'};
                   ?C_IPV6 ->
                       {'urn:ietf:params:xml:ns:yang:ietf-inet-types', 'ipv6-address'};
                   ?C_DATETIME->
                       {'urn:ietf:params:xml:ns:yang:ietf-yang-types', 'date-and-time'};
                   ?C_DATE ->
                       {'http://www.w3.org/2001/XMLSchema', 'date'};
                   ?C_TIME ->
                       {'http://www.w3.org/2001/XMLSchema', 'time'};
                   ?C_DURATION ->
                       {'http://www.w3.org/2001/XMLSchema', 'duration'};
                   ?C_OID ->
                       {'urn:ietf:params:xml:ns:yang:ietf-yang-types', 'object-identifier'};
                   ?C_BINARY ->
                       {'http://www.w3.org/2001/XMLSchema', 'base64Binary'};
                   ?C_IPV4PREFIX ->
                       {'urn:ietf:params:xml:ns:yang:ietf-inet-types', 'ipv4-prefix'};
                   ?C_IPV6PREFIX ->
                       {'urn:ietf:params:xml:ns:yang:ietf-inet-types', 'ipv6-prefix'};
                   ?C_DQUAD ->
                       {'urn:ietf:params:xml:ns:yang:ietf-yang-types', 'dotted-quad'};
                   ?C_HEXSTR ->
                       {'urn:ietf:params:xml:ns:yang:ietf-yang-types', 'hex-string'};
                   ?C_IPV4_AND_PLEN ->
                       {'http://tail-f.com/ns/confd/1.0', ipv4AddressAndPrefixLength};
                   ?C_IPV6_AND_PLEN ->
                       {'http://tail-f.com/ns/confd/1.0', ipv6AddressAndPrefixLength};

                   %% ENUM, QNAME, BIT, INSTANCE_IDENTIFIER, DECIMAL64, IDENTITYREF cannot be (safely) converted
                   _ -> none
               end,
    case TypePair of
        none -> err;
        _ -> econfd_schema:val2str(TypePair, Val)
    end.

int_from_string(Str, Path) ->
    case string:to_integer(Str) of
        {error, _} ->
            ?LOGWARN("Error converting value to integer, returning zero", Path, Str),
            0;
        {Int, []} ->
            Int;
        {Int, _Rest} ->
            ?LOGWARN("Trailing garbage characters found when converting value to integer", Path, Str),
            Int
    end.

convert_int_value(Cs, IntVal, Path) ->
    Int = case IntVal of
              {Tag, VInt} when is_integer(VInt), Tag >= ?C_INT8, Tag =< ?C_UINT64 ->
                  VInt;
              VInt when is_integer(VInt) ->
                  VInt;
              Bin when is_binary(Bin) ->
                  int_from_string(binary_to_list(Bin), Path);
              Str when is_list(Str) ->
                  int_from_string(Str, Path)
          end,
    case Cs#confd_cs_node.primitive_type of
        ?C_INT32 -> Int;
        Type -> {Type, Int}
    end.

convert_enum_value(Val=?CONFD_ENUM_VALUE(_X), _) ->
    Val;
convert_enum_value(IntVal, _) when is_integer(IntVal) ->
    ?CONFD_ENUM_VALUE(IntVal);
convert_enum_value(StrVal, Cs) when is_list(StrVal) ->
    {ok,Val} = econfd_schema:str2val(Cs, StrVal),
    Val;
convert_enum_value(Atom, Cs) when is_atom(Atom) ->
    convert_enum_value(atom_to_list(Atom), Cs).

convert_key_values(Path,Keys) ->
    case econfd_schema:ikeypath2cs(Path) of
        #confd_cs_node{keys=KeyNames} ->
            KeyConv = fun ({KeyName,KeyVal}) ->
                              convert_value([KeyName|Path], KeyVal)
                      end,
            list_to_tuple(lists:map(KeyConv, lists:zip(KeyNames, tuple_to_list(Keys))));
        _ ->
            ?LOGWARN("No schema node found for path", Path),
            Keys
    end.

convert_path([]) ->
    [];
convert_path([Keys|ListPath]) when is_tuple(Keys) ->
    [convert_key_values(ListPath, Keys)|convert_path(ListPath)];
convert_path([P|Path]) ->
    [P|convert_path(Path)].


%%%===================================================================
%%% End
%%%===================================================================
