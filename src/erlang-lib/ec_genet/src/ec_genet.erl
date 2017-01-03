-module(ec_genet).
%%
%% @doc Erlang module providing most of the auxiliary functions to be used to interface
%% with the <em>Generic Transforms</em> module.
%%
%% Apart from the functions (see below), this module defines also the record type {@link
%% mappings()}.  This type is the core of any mapping description - when writing a
%% mapping, all you need to provide is a function returning a {@link mappings()} instance
%% for every (interesting) higher-level keypath.
%%
%% Please refer to <a href="overview-summary.html">overview</a> for more details on the
%% mappings record.
%%
%% @end

-behaviour(application).
-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
%% @headerfile "ec_genet.hrl"
-include("ec_genet.hrl").
-include("nodebug_macros.hrl").
-export([start/2,stop/1,
         start_map/2,start_map/3,stop_map/1,stop_map/2,running_mappings/0,
         exists/2,get_elem/3,get_elem_str/3,get_case/4,get_object/3,
         get_next/1,num_instances/2,composite_list_get_next/4,distribute_value/4,
         keys_by_paths_get_next/4,constant_keyset_get_next/4,
         set_elem/3,set_elem2/3,set_elem_str/3,set_object/3,copy_tree/3,
         create/2,create_if_nonexist/2,create_if_nonexist2/2,
         delete/2,delete_if_exists/2,
         move/3,move_after/3,init_cursor/2,
         get_first_keyset/2,
         cast_to_atom/1,
         prefix_to_addrmask/1,addrmask_to_prefix/2,
         join_pick/2,join_prio/2,join_pick_not_equals/2,
         dup/2,
         str2val/2,val2str/2,
         decimal642int/2, int2decimal64/2,
         joined_lists_next/3,value_fun/1,int_value_fun/1,
         get_next_flat_key/2,delete_flat_instances/3,
         tuple_prefix/2,subtuple/2,subtuple/3,append_tuples/2,
         switch_discriminator/4,
         nested_list_next/4,delete_parent_if_empty/3,
         ordering_get_next/2,
         ip_masklen/1,ipv4_netmask/1,ipv4_invnetmask/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @spec start(_StartMode,_StartArgs) -> {ok, Pid}
start(_StartMode, _StartArgs) ->
    case ec_genet_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @spec stop(_State) -> {ok, Pid}
stop(_State) ->
    ok.

%% @spec start_map(Path, MapFun) -> ok
%% Path = ikeypath()
%% MapFun = (HLPath :: ikeypath()) -> mappings()
%%
%% @doc Register the mapping function for given path prefix. The priority {@link start/3}
%% is length(Path)*100
start_map(Path, MapFun) ->
    start_map(length(Path)*100, Path, MapFun).
%% @spec start_map(Prio, Path, MapFun) -> ok
%% Prio = int()
%%
%% @doc Register the mapping function with given priority. Higher priority value takes
%% precedence in case of overlapping mappings.
start_map(Prio, Path, MapFun) ->
    ok = ec_genet_server:reg_mapping(Prio, Path, MapFun),
    ok.

%% @spec stop_map(Path) -> ok
%%
%% @doc Unregisters the mapping.
stop_map(Path) ->
    stop_map(length(Path)*100, Path).
%% @spec stop_map(Prio, Path) -> ok
%%
%% @doc Unregisters the mapping.
stop_map(Prio, Path) ->
    ok = ec_genet_server:unreg_mapping(Prio, Path),
    ok.

%% @spec running_mappings() -> [[Mapping]]
%% Mapping = {{Prio, Path}, MapFun}
%%
%% @doc Retrieve all registered mappings.
running_mappings() ->
    ets:match(ec_genet_maps,{'$1','$2'}).

%% @spec exists(Tctx, Path) -> true | false
%% Tctx = confd_trans_ctx()
%%
%% @doc Verify whether the path (leaf, container, list instance) exists. Auxiliary
%% function to invoke `econfd_maapi:exists/3' in the current transaction.
exists(Tctx, Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    case econfd_maapi:exists(M, TH, Path) of
        {ok, RawVal} ->
            RawVal;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:exists_optional() returned an error", ErrMsg),
            throw({error, {ec_genet, exists, ErrMsg, Path}})
    end.

%% @spec num_instances(Tctx, Path) -> integer()
%% Tctx = confd_trans_ctx()
%%
%% @doc Get the number of instances.  Auxiliary function to invoke
%% `econfd_maapi:num_instances/3' in the current transaction.
num_instances(Tctx, Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    Ret = 
        case econfd_maapi:num_instances(M, TH, Path) of
            {ok, RawVal} ->
                RawVal;
            {error, ErrMsg} ->
                ?LOGMSG("maapi:num_instances() returned an error", ErrMsg),
                throw({error, {ec_genet, num_instances, ErrMsg, Path}})
        end,
    Ret.

%% @spec get_elem_str(Tctx, Path, None) -> string() | None
%%
%% @doc Get the string representation of the leaf value. Invoke `econfd_maapi:get_elem/3'
%% in the current transaction, return a string representation of the value if found, None
%% if the leaf does not exists.
get_elem_str(Tctx, Path, None) ->
    case get_elem(Tctx, Path, None) of
        None ->
            None;
        Val ->
            val2str(Path, Val)
    end.

%% @spec get_elem(Tctx, Path, None) -> value() | None
%%
%% @doc Get the leaf value. Invoke `econfd_maapi:get_elem/3' in the current transaction,
%% return the value if found, None if the leaf does not exists.
get_elem(Tctx, Path, None) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    case econfd_maapi:get_elem(M, TH, Path) of
        {ok, RawVal} ->
            RawVal;
        {error, {1,_ErrMsg}} ->
            None;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:get_elem() returned an error", ErrMsg),
            throw({error, {ec_genet, get_elem, ErrMsg, Path}})
    end.

%% @spec get_case(Tctx, Path, Choice, None) -> qtag() | None
%% Choice = qtag() | [qtag()]
%%
%% @doc Get the case name selected in the indicated choice. Invoke
%% `econfd_maapi:get_case/4' in the current transaction, return the selected case or None
%% if the choice does not exists.
get_case(Tctx, Path, Choice, None) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    case econfd_maapi:get_case(M, TH, Path, Choice) of
        {ok, RawVal} ->
            RawVal;
        {error, {1,_ErrMsg}} ->
            None;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:get_case() returned an error", ErrMsg),
            throw({error, {ec_genet, get_case, ErrMsg, Path, Choice}})
    end.

%% @spec get_object(Tctx, Path, None) -> [value()] | None
%%
%% @doc Get the container contents.  Invoke `econfd_maapi:get_object/3' in the current
%% transaction, return the container (or list instance) contents or None if the path does
%% not exists.
get_object(Tctx, Path, None) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    case econfd_maapi:get_object(M, TH, Path) of
        {ok, RawVal} ->
            RawVal;
        {error, {1,_ErrMsg}} ->
            None;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:get_object() returned an error", ErrMsg),
            throw({error, {ec_genet, get_object, ErrMsg, Path}})
    end.

%% @spec set_elem_str(Tctx, Path, StrValue :: string()) -> ok
%%
%% @doc Set the leaf `Path' to the value with string representation StrValue. Invoke
%% `econfd_maapi:set_elem/4' in the current transaction.
set_elem_str(Tctx, Path, StrValue) ->
    LLVal = str2val(Path, StrValue),
    set_elem(Tctx, Path, LLVal).

%% @spec set_elem(Tctx, Path, Value :: value()) -> ok
%%
%% @doc Set the leaf `Path' to the value `Value'. Invoke `econfd_maapi:set_elem/4' with a
%% transaction handle.
set_elem(Tctx, Path, Value) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CVal = ec_genet_server:convert_value(Path, Value),
    case econfd_maapi:set_elem(M, TH, Path, CVal) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:set_elem() returned an error", ErrMsg),
            throw({error, {ec_genet, set_elem, ErrMsg, Path, Value}})
    end.

%% @spec set_elem2(Tctx, Path, Value :: value()) -> ok
%%
%% @doc Set the leaf `Path' to the value `Value'. Invoke `econfd_maapi:set_elem/4' with a
%% transaction handle. If parent instance for element does not exist then ignore error
set_elem2(Tctx, Path, Value) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CVal = ec_genet_server:convert_value(Path, Value),
    case econfd_maapi:set_elem(M, TH, Path, CVal) of
        ok ->
            ok;
        {error, {?CONFD_ERR_NOEXISTS, _}} ->
            ?LOGMSG("Parent instance did not exist, so set_elem2 did nothing", Path),
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:set_elem() returned an error", ErrMsg),
            throw({error, {ec_genet, set_elem, ErrMsg, Path, Value}})
    end.

%% @spec set_object(Tctx, Path, Values :: [value()]) -> ok
%%
%% @doc Set the contents of the container `Path' to the value list `Values'.  Invoke
%% `econfd_maapi:set_object/4' with a transaction handle.
set_object(Tctx, Path, Values) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    case econfd_maapi:set_object(M, TH, Path, Values) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:set_object() returned an error", ErrMsg),
            throw({error, {ec_genet, set_object, ErrMsg, Path, Values}})
    end.

%% @spec copy_tree(Tctx, From, To) -> ok
%%
%% @doc Copy the contents of the node `From' to the node `To'.  Invoke
%% `econfd_maapi:copy_tree/4' with a transaction handle.
copy_tree(Tctx, From, To) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    case econfd_maapi:copy_tree(M, TH, From, To) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:copy_tree() returned an error", ErrMsg),
            throw({error, {ec_genet, copy_tree, ErrMsg, From, To}})
    end.

%% @spec create(Tctx, Path) -> ok
%%
%% @doc Create the path (presence container, list instance, or a empty-typed
%% leaf). Invoke `econfd_maapi:create/3' in the current transaction.
create(Tctx,Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:create(M, TH, CPath) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:create() returned an error", ErrMsg),
            throw({error, {ec_genet, create, ErrMsg, CPath}})
    end.

%% @spec create_if_nonexist(Tctx, Path) -> ok
%%
%% @doc Create the path (presence container, list instance, or a empty-typed
%% leaf). If element already exists, ignore the error.
%%  Invoke `econfd_maapi:create/3' in the current transaction.
create_if_nonexist(Tctx,Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:create(M, TH, CPath) of
        ok ->
            ok;
        {error, {?CONFD_ERR_ALREADY_EXISTS, _}} ->
            ?LOGMSG("Element already existed, so create_if_nonexist did nothing", CPath),
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:create() returned an error", ErrMsg),
            throw({error, {ec_genet, create, ErrMsg, CPath}})
    end.

%% @spec create_if_nonexist2(Tctx, Path) -> ok
%%
%% @doc Create the path (presence container, list instance, or a empty-typed
%% leaf). If element already exists, ignore the error. If parent path did
%% not exists then ignore error.
%%  Invoke `econfd_maapi:create/3' in the current transaction.
create_if_nonexist2(Tctx,Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:create(M, TH, CPath) of
        ok ->
            ok;
        {error, {?CONFD_ERR_NOEXISTS, _}} ->
            ?LOGMSG("Parent instance did not exist, so create_if_nonexist did nothing", CPath),
            ok;
        {error, {?CONFD_ERR_ALREADY_EXISTS, _}} ->
            ?LOGMSG("Element already existed, so create_if_nonexist did nothing", CPath),
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:create() returned an error", ErrMsg),
            throw({error, {ec_genet, create, ErrMsg, CPath}})
    end.


%% @spec delete(Tctx, Path) -> ok
%%
%% @doc Delete the path and whatever is below it. Invoke `econfd_maapi:delete/3' in the
%% current transaction.
delete(Tctx,Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:delete(M, TH, CPath) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:delete() returned an error", ErrMsg),
            throw({error, {ec_genet, delete, ErrMsg, CPath}})
    end.

%% @spec delete_if_exists(Tctx, Path) -> ok
%%
%% @doc Delete the path and whatever is below it. Do not return
%% an error if it does not exist. Invoke `econfd_maapi:delete/3' in the
%% current transaction.
delete_if_exists(Tctx,Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:delete(M, TH, CPath) of
        ok ->
            ok;
        {error, {?CONFD_ERR_NOEXISTS, _}} ->
            ?LOGMSG("Element did not exist, so delete_if_exists did nothing", CPath),
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:delete() returned an error", ErrMsg),
            throw({error, {ec_genet, delete, ErrMsg, CPath}})
    end.


%% @spec move(Tctx, Path, NewKey :: key()) -> ok
%%
%% @doc Move (rename) a path - list instance to the new key value.
move(Tctx,Path,NewKey) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:move(M, TH, CPath, NewKey) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:move() returned an error", ErrMsg),
            throw({error, {ec_genet, move, ErrMsg, CPath, NewKey}})
    end.

%% @spec move_after(Tctx, Path, PrevKeys :: key()) -> ok
%%
%% @doc Move instance in the list
move_after(Tctx, Path, PrevKeys) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    CPath = ec_genet_server:convert_path(Path),
    case econfd_maapi:move_ordered(M, TH, CPath, PrevKeys) of
        ok ->
            ok;
        {error, ErrMsg} ->
            ?LOGMSG("maapi:move_ordered() returned an error", ErrMsg),
            throw({error, {ec_genet, move_after, ErrMsg, CPath, PrevKeys}})
    end.

%% @spec init_cursor(Tctx, Path) -> cursor()
%%
%% @doc Initialize the list cursor for subsequent use in `ec_genet:get_next/1'.
init_cursor(Tctx, Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    econfd_maapi:init_cursor(M, TH, Path).

get_next(Cursor) ->
    case econfd_maapi:get_next(Cursor) of
        {ok, NewKeys, NewCursor} -> {NewKeys,NewCursor};
        _                        -> not_found
    end.

composite_list_get_next(Tctx,HLPath,-1,Extra) ->
    {composite_list_get_next, ListOfYangLists} = lists:keyfind(composite_list_get_next, 1, Extra),
    State = composite_list_initial_state(Tctx, ListOfYangLists, Extra),
    composite_list_get_next(Tctx,HLPath,{State},Extra);
composite_list_get_next(Tctx,_HLPath,{States},Extra) ->
    {composite_list_get_next_mode, Mode} = lists:keyfind(composite_list_get_next_mode, 1, Extra),
    SortedStates = lists:sort(fun({Key1,_,_}, {Key2,_,_}) -> Key1 < Key2 end,
                              lists:filter(fun (State) -> State /= {false,undefined} end, States)),
    case {Mode, SortedStates} of
        {_,[]} ->
            %% All KeySets were false, i.e. end of list
            {false, undefined};
        {each,[State={Key,_,_}|Rest]} ->
            {Key, {[composite_list_advance_state(Tctx, State, Extra)|Rest]}};
        %% {intersection, _} -> FIXME to be implemented
        {union,NStates=[{NextKeySet,_,_}|_]} ->
            NextStates = lists:map(fun(State) -> composite_list_advance_state_union(Tctx, NextKeySet, State, Extra) end,
                                   NStates),
            {NextKeySet, {NextStates}}
    end.

composite_list_advance_state_union(Tctx, KeySet, State={KeySet,_,_}, Extra) ->
    composite_list_advance_state(Tctx, State, Extra);
composite_list_advance_state_union(_, _, State, _) ->
    State.

composite_list_advance_state(Tctx, {_,Cursor,Path}, Extra) ->
    case get_next(Cursor) of
        {Key,NewCursor} ->
            case (composite_list_filter_fn(Extra))(Tctx, Key, Path) of
                false -> composite_list_advance_state(Tctx, {Key,NewCursor,Path}, Extra);
                _ -> {(composite_list_key_fn(Extra))(Key,Path),NewCursor,Path}
                         end;
        not_found -> {false, undefined}
    end.

composite_list_initial_state(Tctx, [TopListPath|ListPaths], Extra) ->
    States = composite_list_initial_state(Tctx,ListPaths,Extra),
    Cursor = init_cursor(Tctx, TopListPath),
    State = composite_list_advance_state(Tctx, {-1, Cursor, TopListPath}, Extra),
    [State|States];
composite_list_initial_state(_Tctx, [], _) ->
    [].

composite_list_extra_attr(Extra, Attr, Def) ->
    case lists:keyfind(Attr, 1, Extra) of
        false -> Def;
        {Attr, Val} -> Val
    end.

composite_list_key_fn(Extra) ->
    composite_list_extra_attr(Extra, composite_list_key_fn, fun(Key,_) -> Key end).

composite_list_filter_fn(Extra) ->
    composite_list_extra_attr(Extra, composite_list_filter_fn, fun(_Tctx,_Keys,_Path) -> true end).

%% @spec val2str(Path, Val) -> binary()
%%
%% @doc Return the binary string representation of the value. The representation depends
%% on the declared type of the leaf in the Yang data model.
val2str(Path, Val) ->
    Type = econfd_schema:ikeypath2cs(Path),
    {ok, StrBin} = econfd_schema:val2str(Type, Val),
    StrBin.

%% @spec str2val(Path, StrVal) -> value()
%% StrVal = string() | binary() | atom()
%%
%% @doc Return the ConfD value from the string representation.
str2val(Path, StrAtom) when is_atom(StrAtom)->
    str2val(Path,list_to_binary(atom_to_list(StrAtom)));
str2val(Path, StrList) when is_list(StrList)->
    str2val(Path,list_to_binary(StrList));
str2val(Path, StrBin) when is_binary(StrBin)->
    Type = econfd_schema:ikeypath2cs(Path),
    {ok, Val} = econfd_schema:str2val(Type, StrBin),
    Val.

%% @spec int2decimal64(value(), int()) -> value()
%%
%% @doc Convert integer ConfD value to decimal ConfD value with given number of decimals.
%% For instance, integer 123 with 2 decimals is converted to value 123.00.
int2decimal64({_,Val},Decimals) ->
    ?CONFD_DECIMAL64({int2decimal64_int(Val,Decimals),Decimals}).
int2decimal64_int(Val,0) ->
    Val;
int2decimal64_int(Val,Decimals) when Decimals > 0 ->
    int2decimal64_int(Val*10,Decimals-1).

%% @spec decimal642int(value(), PowerTen :: int()) -> value()
%%
%% @doc Convert decimal ConfD value to integer. If PowerTen = 0, it is an inverse (save
%% for the return type) of {@link int2decimal64/0}.
decimal642int(?CONFD_DECIMAL64({Val,Decimals}),Decimals) ->
    Val;
decimal642int(?CONFD_DECIMAL64({Val,Decimals}),PowerTen) when Decimals > PowerTen ->
    decimal642int(?CONFD_DECIMAL64({Val div 10,Decimals-1}), PowerTen);
decimal642int(?CONFD_DECIMAL64({Val,Decimals}),PowerTen) when Decimals < PowerTen ->
    decimal642int(?CONFD_DECIMAL64({Val*10,Decimals+1}), PowerTen).

cast_to_atom(BinStr) when is_binary(BinStr) ->
    list_to_atom(binary_to_list(BinStr));
cast_to_atom(ListStr) when is_list(ListStr) ->
    list_to_atom(ListStr);
cast_to_atom(Atom) when is_atom(Atom) ->
    Atom.

join_prio(N,List) ->
    Selected = lists:nth(N, List),
    [Head|Tail] = List,
    case lists:all(fun(Elem)->Elem==Head end, Tail) of
        true -> ok;
        _ ->
            ?LOGMSG("Warning: The mapping declares all these values"
                    "should be the same, but the aren't. Pretending "
                    "they are all equal to the selected element.",
                    [Selected, List]),
            problematic
    end,
    Selected.

join_pick(N,List) ->
    lists:nth(N, List).

join_pick_not_equals(NotEquals,[Elem|List]) when Elem == NotEquals->
    join_pick_not_equals(NotEquals,List);
join_pick_not_equals(NotEquals,[Elem|_List]) when Elem /= NotEquals->
    Elem;
join_pick_not_equals(NotEquals,[]) ->
    NotEquals.

dup(N,Val) ->
    lists:duplicate(N,Val).

get_first_keyset(Tctx, Path) ->
    M = ec_genet_server:tctx_maapi_sock(Tctx),
    TH = ec_genet_server:tctx_maapi_thandle(Tctx),
    Cursor = econfd_maapi:init_cursor(M, TH, Path),
    case econfd_maapi:get_next(Cursor) of
        {ok, Keys, _NewCursor} -> Keys;
        _                      -> not_found
    end.


%% @spec joined_lists_next([Prefix], CommonPath, KeyConv) -> (Tctx, HLPath, Next, Extra) -> {false, undefined} | {Key, Next}
%% KeyConv = (Tctx, Prefix, key()) -> key()
%%
%% @doc Return a function that can be used for the `get_next' mappings field. The returned
%% function iterates over lists given by Prefixes and returns key processed by
%% KeyConv. KeyConv is a function that receives Tctx, the path prefix from which the value
%% originates, and the key value, and is supposed to return a key value.
joined_lists_next(Prefixes, CommonPath, KeyConv) ->
    fun(Tctx, _, Next, _) ->
            case Next of
                -1 -> run_initiate_dynamic_next(Tctx, Prefixes, CommonPath, KeyConv);
                {_List, _LL, _Cursor} -> get_dynamic_list_next(Tctx, Next, CommonPath, KeyConv)
            end
    end.

get_dynamic_list_next(Tctx, {List, LL, Cursor}, CommonPath, KeyConv) ->
    case econfd_maapi:get_next(Cursor) of
        done -> run_initiate_dynamic_next(Tctx, LL, CommonPath, KeyConv);
        {ok, V, NCursor} -> {KeyConv(Tctx, List, V), {List, LL, NCursor}}
    end.

run_initiate_dynamic_next(_Tctx, [], _, _) ->
    {false, undefined};
run_initiate_dynamic_next(Tctx, [Prefix|Rest], CommonPath, KeyConv) ->
    get_dynamic_list_next(Tctx, {Prefix, Rest, ec_genet:init_cursor(Tctx, Prefix ++ CommonPath)}, CommonPath, KeyConv).

%% @spec get_next_flat_key(Path, integer()) -> NextFun
%% NextFun = (Tctx, Path, -1 | FlatCursor, Extra) -> {false, undefined} | {key(), FlatCursor}
%% FlatCursor = {cursor(), [value()]}
%%
%% @doc `get_next' function generator intended for use for mapping nested lists to flat
%% lists.  Used in NESTED_TO_FLAT macro.
get_next_flat_key(LLFlatList, Count) ->
    fun(Tctx,_,Next,_) ->
            case Next of
                -1 -> next_flat_key_initiate(Tctx, LLFlatList, Count);
                {Cursor, Last} -> next_flat_key(Cursor, Last)
            end
    end.

next_flat_key_initiate(Tctx, LLFlatList, Count) ->
    Cursor = init_cursor(Tctx, LLFlatList),
    next_flat_key(Cursor, lists:duplicate(Count, none)).

next_flat_key(Cursor, Last) ->
    case get_next(Cursor) of
        {Keys, NCursor} ->
            case lists:sublist(tuple_to_list(Keys), length(Last)) of
                Part when Part /= Last ->
                    {list_to_tuple(Part), {NCursor, Part}};
                _ -> next_flat_key(NCursor, Last)
            end;
        not_found ->
            {false, undefined}
    end.

%% @spec delete_flat_instances(Tctx, Path, {value()}) -> ok
%%
%% @doc Delete all flat list instances whose keys start with the given tuple.
delete_flat_instances(Tctx, LLFlatList, Keys) ->
    delete_flat_instances(Tctx, LLFlatList, Keys, init_cursor(Tctx, LLFlatList)).

delete_flat_instances(Tctx, LLFlatList, Keys, Cursor) ->
    ?LOGMSG("Key is", Keys),

    case econfd_maapi:find_next(Cursor, ?CONFD_FIND_SAME_OR_NEXT, Keys) of
        {ok, FlatKeys, NCursor} ->
            {NxtKey, _} = FlatKeys,
            ?LOGMSG("NxtKey is", NxtKey),
            case {NxtKey} == Keys of
                true ->
                    ec_genet:delete(Tctx, [FlatKeys|LLFlatList]),
                    delete_flat_instances(Tctx, LLFlatList, Keys, NCursor);
                _ -> ok
            end;
        _ ->
            ok
    end.

%% @spec value_fun(ValFun) -> (Tctx, HLPath, Value, Extra) -> Value
%% ValFun = (Value) -> Value
%%
%% @doc For a simple value convertor generate a function that can be used for
%% fupval/fdnval fields. Note that ValFun needs to accept the ConfD representation of the
%% value, including the tag, if present.
value_fun(ValFun) ->
    fun(_, _, not_found, _) ->
            not_found;
       (_, _, V, _) ->
            ValFun(V)
    end.

%% @spec int_value_fun(ValFun) -> (Tctx, HLPath, Value, Extra) -> Value
%% ValFun = (IntValue) -> IntValue
%%
%% @doc Special case of {@link value_fun} for convenient handling of integer values,
%% without the necessity to process the type tag.  Assumes that both HL and LL values are
%% of the same integer type (e.g. both are uint8, or both are int64, etc.).
int_value_fun(ValFun) ->
    fun(_, _, not_found, _) ->
            not_found;
       (_, _, {Type, V}, _) ->
            {Type, ValFun(V)};
       (_, _, V, _) when is_integer(V) ->
            ValFun(V)
    end.

%% tuple functions (useful when dealing with keys)

%% @spec tuple_prefix(tuple(), tuple()) -> boolean()
%%
%% @doc Returns `true' if Tuple1 is a prefix (in the list sense) of Tuple2.
tuple_prefix(Tuple1, Tuple2) ->
    lists:prefix(tuple_to_list(Tuple1), tuple_to_list(Tuple2)).

%% @spec subtuple(tuple(), integer()) -> tuple()
%%
%% @doc Return a tuple of size Len which is a prefix of Tuple.
subtuple(Tuple, Len) ->
    list_to_tuple(lists:sublist(tuple_to_list(Tuple), Len)).

%% @spec subtuple(tuple(), integer(), integer()) -> tuple()
%%
%% @doc Return a tuple of size Len which is contained in Tuple starting at the position
%% Start.  `subtuple(T, 1, L)' is equivalent to `subtuple(T, L)'.
subtuple(Tuple, Start, Len) ->
    list_to_tuple(lists:sublist(tuple_to_list(Tuple), Start, Len)).

%% @spec append_tuples(tuple(), tuple()) -> tuple()
%%
%% @doc Return a tuple containing all elements of Tuple1 and Tuple2, in this order.
append_tuples(Tuple1, Tuple2) ->
    list_to_tuple(tuple_to_list(Tuple1) ++ tuple_to_list(Tuple2)).

%% @spec switch_discriminator(Tctx, Path, [Path], [Path]) -> ok
%%
%% @doc Copy existing contents to newly created nodes.  Useful in various
%% discriminator switching scenarios.  `PrefixPath' is a path common to all
%% nodes, `AllPaths' is a list of all possible suffixes (such that `[Suffix ++
%% PrefixPath || Suffix <- AllPaths]' is the set of all paths to the possible
%% destination nodes; `NewPaths' is a list of the suffixes that should
%% exist. All nodes not referred to in `NewPaths' are deleted, for nodes in
%% `NewPaths' that did not exist before the contents is copied from the first
%% existing node (if any).
switch_discriminator(Tctx, PrefixPath, AllPaths, NewPaths) ->
    ExSet = [Path || Path <- AllPaths, exists(Tctx, Path ++ PrefixPath)],
    NewSet = [Path || Path <- NewPaths, not lists:member(Path, ExSet)],
    DelSet = [Path || Path <- ExSet, not lists:member(Path, NewPaths)],
    case ExSet of
        [] -> ok;
        [P|_] -> lists:map(fun(Q) -> copy_container(Tctx, P++PrefixPath, Q++PrefixPath) end, NewSet)
    end,
    lists:map(fun(Path) -> delete(Tctx, Path++PrefixPath) end, DelSet),
    ok.

copy_container(Tctx, From, To) ->
    % TODO: should be just copy_tree, but this is broken in 5.4
    Cs = econfd_schema:ikeypath2cs(From),
    case Cs#confd_cs_node.flags band (?CONFD_CS_IS_LIST bor ?CONFD_CS_IS_CONTAINER) of
        0 ->
            % get_object cannot be called on leaves
            Val = get_elem(Tctx, From, not_found),
            set_elem(Tctx, To, Val);
        _ ->
            Contents = get_object(Tctx, From, []),
            set_object(Tctx, To, Contents)
    end.

%% @spec ordering_get_next(Path, ({value()}) -> value()) -> NextFun
%%
%% @doc Return a `get_next' function that converts the key values and orders them
%% according to their natural ordering.
%% FIXME: for larger lists this may be slow!
ordering_get_next(LLPath, ConvFun) ->
    fun(Tctx,_,-1,_) ->
            AllEntries = iterate_cursor(init_cursor(Tctx, LLPath), ConvFun),
            ordered_entries_get_next(lists:sort(AllEntries));
       (_,_,Entries,_) ->
            ordered_entries_get_next(Entries)
    end.

ordered_entries_get_next([]) ->
    {false, undefined};
ordered_entries_get_next([E|Entries]) ->
    {E, Entries}.

iterate_cursor(C, ConvFun) ->
    case get_next(C) of
        {Entry, NC} ->
            [ConvFun(Entry)|iterate_cursor(NC, ConvFun)];
        not_found ->
            []
    end.

%% @spec nested_list_next(Tctx, Path, Path, -1 | NestedCursor) -> {value(), NestedCursor}
%% NestedCursor = {Cursor, {value()}, Cursor}
%%
%% @doc Iterates over two levels of nested lists.  The paths in the arguments are the path
%% to the outer list, and the relative path to the inner list.  The return value is a key
%% tuple (created as a concatenation of the two key tuples from the two lists) and a
%% "nested cursor" - cursors for the two list and the key value for the outer list.
nested_list_next(Tctx, LList, SubList, -1) ->
    nested_list_next(Tctx, LList, SubList, {init_cursor(Tctx, LList), false, undefined});
nested_list_next(_Tctx, _LList, _SubList, {undefined, _, _}) ->
    {false, undefined};
nested_list_next(Tctx, LList, SubList, {C=#maapi_cursor{}, _, undefined}) ->
    case econfd_maapi:get_next(C) of
        done ->
            {false, undefined};
        {ok, K1, NC} ->
            nested_list_next(Tctx, LList, SubList, {NC, K1, init_cursor(Tctx, SubList ++ [K1|LList])})
    end;
nested_list_next(Tctx, LList, SubList, {C1, K1, C2}) ->
    case econfd_maapi:get_next(C2) of
        done ->
            nested_list_next(Tctx, LList, SubList, {C1, K1, undefined});
        {ok, K2, NC2} ->
            {append_tuples(K1, K2), {C1, K1, NC2}}
    end.

%% @spec delete_parent_if_empty(Tctx, Path, Path) -> ok
%%
%% @doc Delete the outer list instance if there are no inner list instances.  This may
%% often be needed in case of mapping flat list to nested lists - deletion of HL flat list
%% instances is mapped to deletion of inner LL list instances, but it might be necessary
%% to delete childless outer LL list instances.
delete_parent_if_empty(Tctx, LListInst, SubList) ->
    case num_instances(Tctx, SubList ++ LListInst) of
        0 -> delete(Tctx, LListInst);
        _ -> ok
    end.

%% @spec distribute_value(Tctx, Value, [Path], Path) -> ok
%%
%% @doc Set the nodes in all existing instances of the lists to given value.
distribute_value(Tctx, Value, ListPaths, SubPath) ->
    [distribute_value_to_list(Tctx, Value, ListPath, SubPath) || ListPath <- ListPaths],
    ok.

distribute_value_to_list(Tctx, Value, ListPath, SubPath) ->
    C = init_cursor(Tctx, ListPath),
    iterate_cursor(C, fun(Key) -> set_elem(Tctx, SubPath ++ [Key|ListPath], Value) end).

%% @spec keys_by_paths_get_next(Tctx, Path, -1 | [{key(), Path}], [Path]) -> {value(), [{key(), Path}]}
%%
%% @doc `get_next' implementation that returns keys corresponding to existing paths
%% provided in `Extra'. `Extra' is a list of pairs `{key, LL path}' which are tested one
%% by one, and for each path that exists in the transaction the key is provided in given
%% `get_next' iteration.
keys_by_paths_get_next(Tctx,HLPath,-1,Extra) ->
    keys_by_paths_get_next(Tctx,HLPath,Extra,Extra);
keys_by_paths_get_next(_Tctx,_HLPath,[],_) ->
    {false,undefined};
keys_by_paths_get_next(Tctx,HLPath,[{Key,Path}|Rest],Extra) ->
    CPath = ec_genet_server:convert_path(Path),
    case ec_genet:exists(Tctx,CPath) of
        true ->
            {Key,Rest};
        false ->
            keys_by_paths_get_next(Tctx,HLPath,Rest,Extra)
    end.

%% @spec constant_keyset_get_next(Tctx, Path, -1 | [{key(), Path}], [Path]) -> {value(), [{key(), Path}]}
%%
%% @doc `get_next` implementation returning set of key values given as extra argument.
constant_keyset_get_next(Tctx, Path, -1, ValFun) when is_function(ValFun) ->
    constant_keyset_get_next(Tctx, Path, -1, ValFun(Tctx));
constant_keyset_get_next(Tctx, Path, -1, List) ->
    constant_keyset_get_next(Tctx, Path, List, List);
constant_keyset_get_next(_, _, [], _) ->
    {false, undefined};
constant_keyset_get_next(_, _, [Keys|List], _) ->
    {Keys, List}.

prefix_to_addrmask(?CONFD_IPV4PREFIX({Addr, Len})) ->
    {?CONFD_IPV4(Addr), ?CONFD_IPV4(list_to_tuple(ipv4_netmask(Len)))};
prefix_to_addrmask(?CONFD_IPV6PREFIX({Addr, Len})) ->
    {?CONFD_IPV6(Addr), ?CONFD_IPV6(list_to_tuple(ipv6_netmask(Len)))}.

addrmask_to_prefix(?CONFD_IPV4(Addr), ?CONFD_IPV4(Mask)) when size(Addr) == 4 ->
    ?CONFD_IPV4PREFIX({Addr, ip_masklen(tuple_to_list(Mask))});
addrmask_to_prefix(?CONFD_IPV6(Addr), ?CONFD_IPV6(Mask)) when size(Addr) == 16 ->
    ?CONFD_IPV6PREFIX({Addr, ip_masklen(tuple_to_list(Mask))}).

ipv4_netmask(Len) ->
    to_mask(16#ffffffff - (16#ffffffff bsr Len), 4, []).

ipv4_invnetmask(Len) ->
    [A,B,C,D] = ipv4_netmask(Len),
    [255-A,255-B,255-C,255-D].

ipv6_netmask(Len) ->
    to_mask(16#ffffffffffffffffffffffffffffffff - (16#ffffffffffffffffffffffffffffffff bsr Len), 16, []).

to_mask(_, 0, L) ->
    L;
to_mask(Num, I, L) ->
    to_mask(Num bsr 8, I-1, [Num band 16#ff | L]).

ip_masklen([255|L]) ->
    8+ip_masklen(L);
ip_masklen([]) ->
    0;
ip_masklen([N|_]) -> mask_bitsize(N).

mask_bitsize(0) -> 0;
mask_bitsize(N) -> 1+mask_bitsize((N bsl 1) band 16#ff).

%%%===================================================================
%%% End
%%%===================================================================
