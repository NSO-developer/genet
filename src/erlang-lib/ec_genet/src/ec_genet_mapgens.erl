%% @doc Functions for generating and processing mapping records.
%%
%% This module contains mapping generators, that are supposed to be used to shorten
%% mapping descriptions.  See ec_map_example module for examples.
%%
%% @copyright 2015 Cisco Systems/Tail-f Systems.  All rights reserved.

-module(ec_genet_mapgens).
-export([constant/2,constant/3,scale_factor/2,contract_factor/2,
         offset_value/2,enumeration_map/2,choice_map/2,address_map/2,from_cases/2,
         mapping_table_funs/2, mapping_table_funs/1,
         enumeration_map/3,address_map/3,from_cases/3,
         exists_constant/2,sentinel_hl/2,sentinel_hl/3,sentinel_ll/2,sentinel_ll/3,
         to_existence/4,to_existence/3,
         from_existence/4,from_existence/3,from_existence_delete/3,from_existence_delete/2,
         pre_hook/2,post_hook/2,address_switch/2,leaf_list_map/3,
         guard_by_value/3,merge_mappings/2,compose/2,
         existence_to_list_instance/1,leaf_to_list_instance/2,node_to_list_instance_subnode/2,
         leaf_list_to_leaf/1,composite_list/1,mappings_switch/3,composite_list/2]).

-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
-include("ec_genet.hrl").
-include("debug_macros.hrl").

-type path() :: [any()].
-type path_or_map() :: path() | #mappings{}.

%% @spec constant(Path, Value) -> mappings()
%%
%% @doc Generate a constant mappings record, always providing the value.  Note the HLPath
%% argument - it requires the HLPath for converting the value, if needed, and for proper
%% formatting of error messages.
%%
%% This mapping cannot be composed.
constant(HLPath, ConstValue) ->
    CValue = if is_list(hd(HLPath)) andalso is_list(tl(hd(HLPath))) -> ConstValue;
                true -> ec_genet_server:convert_value(HLPath, ConstValue)
             end,
    StrVal = binary_to_list(ec_genet:val2str(HLPath,CValue)),
    Strs = io_lib:format("Only the value ~s can be supported",
                         [StrVal]),
    constant_(CValue, lists:flatten(Strs)).

%% @spec constant(Path, Value, string()) -> mappings()
%%
%% @doc Generate a constant mappings record, always providing the value, with custom error
%% message.  See {@link convert_value/2}.
%%
%% This mapping cannot be composed.
constant(HLPath, ConstValue, ErrorMessage) ->
    CValue = ec_genet_server:convert_value(HLPath, ConstValue),
    constant_(CValue, ErrorMessage).

constant_(CValue, ErrorMessage) ->
    #mappings{get_elem=fun(_,_,_) -> CValue end,
              get_case=fun(_,_,_,_) -> CValue end,
              set_elem=fun(_,_,Val,_) -> case Val of
                                             CValue -> ok;
                                             _ -> throw({error, ErrorMessage})
                                         end
                       end,
              set_case=fun(_,_,_,Case,_) -> case Case of
                                             CValue -> ok;
                                             _ -> throw({error, ErrorMessage})
                                         end
                       end,

              delete=fun(_,_,_) -> ok end}.

%% @spec scale_factor(LLPath | mappings(), number()) -> mappings()
%% LLPath = Path
%%
%% @doc Generate a scaling mapping with given scale factor.  Can be composed with another
%% mapping if the mapping is provided as a first argument - in that case, when mapping
%% down, scaling is applied first, than the other mapping.
scale_factor(LLPathOrMap, Factor) ->
    Map = #mappings{fupval=ec_genet:int_value_fun(fun(LLVal) -> round(LLVal/Factor) end),
                    fdnval=ec_genet:int_value_fun(fun(HLVal) -> round(HLVal*Factor) end)},
    compose(Map, LLPathOrMap).

%% @spec contract_factor(Path, number()) -> mappings()
%% LLPathOrMap = LLPath | mappings()
%%
%% @doc Generate a contracting mapping with given scale factor.  See also
%% {@link scale_factor}.
contract_factor(LLPathOrMap, Factor) ->
    Map = #mappings{fupval=ec_genet:int_value_fun(fun(LLVal) -> round(LLVal*Factor) end),
                    fdnval=ec_genet:int_value_fun(fun(HLVal) -> round(HLVal/Factor) end)},
    compose(Map, LLPathOrMap).

%% @spec offset_value(Path | mappings(), number()) -> mappings()
%%
%% @doc Generate a offset-adding mapping with given offset.  See also {@link
%% scale_factor}.
offset_value(LLPathOrMap, Offset) ->
    Map = #mappings{fupval=ec_genet:int_value_fun(fun (LLVal) -> LLVal-Offset end),
                    fdnval=ec_genet:int_value_fun(fun (HLVal) -> HLVal+Offset end)},
    compose(Map, LLPathOrMap).

%% @spec enumeration_map(Path, EnumPairs) -> mappings()
%% LLPathOrMap = LLPath | mappings()
%% EnumPairs = [tuple()]
%%
%% @doc Enumeration mapping generator.  The second argument is a list of high and low
%% enumeration value pairs.  Can be composed (?).  Special case of {@link address_map/2}.
enumeration_map(LLPathOrMap, HiLoEnumPairs) ->
    address_map(LLPathOrMap, [{?CONFD_ENUM_VALUE(E1),?CONFD_ENUM_VALUE(E2)} || {E1,E2} <- HiLoEnumPairs]).


%% @spec enumeration_map(Path, EnumPairs, EnumPairs) -> mappings()
%% LLPathOrMap = LLPath | mappings()
%% EnumPairs = [tuple()]
%%
%% @doc Variant of {@link enumeration_map/2} accepting lists of enum pairs for both
%% directions.
enumeration_map(LLPathOrMap, HiLoEnumPairs, LoHiEnumPairs) ->
    address_map(LLPathOrMap,
                [{?CONFD_ENUM_VALUE(E1),?CONFD_ENUM_VALUE(E2)} || {E1,E2} <- HiLoEnumPairs],
                [{?CONFD_ENUM_VALUE(E1),?CONFD_ENUM_VALUE(E2)} || {E1,E2} <- LoHiEnumPairs]).

%% @spec address_map(LLPathOrMap, AddrPairs) -> mappings()
%% LLPathOrMap = LLPath | mappings()
%% AddrPairs = [tuple()]
%%
%% @doc General item (address, case, ...) mapping generator.  The second argument is a
%% list of high/low item names.  Can be composed.
%%
%% The item map need not to be bijective, i.e. the items in the item list need not to
%% be unique.  But in such case, the ordering of items in the list matters; the rule is
%% that the latter overrides the former.  For example, item list like
%%
%% `[{a, x1}, {a, x2}]'
%%
%% produces mapping that rewrites LL items `x1' and `x2' both to HL item `a', whereas HL item
%% `a' is rewritten only to `x2'.
address_map(LLPathOrMap, HiLoAddressPairs) ->
    address_map(LLPathOrMap, HiLoAddressPairs, [{J,I} || {I,J} <- HiLoAddressPairs]).

%% @spec address_map(LLPathOrMap, AddrPairs, AddrPairs) -> mappings()
%% LLPathOrMap = LLPath | mappings()
%% AddrPairs = [tuple()]
%%
%% @doc Variant of {@link address_map/2} accepting separate lists of value pairs for the
%% two directions, one list for HL to LL, one for LL to HL.
address_map(LLPathOrMap, HiLoAddressPairs, LoHiAddressPairs) ->
    {UpFun, DnFun} = mapping_table_funs(HiLoAddressPairs, LoHiAddressPairs),
    Map = #mappings{fupval=ec_genet:value_fun(UpFun), fdnval=ec_genet:value_fun(DnFun)},
    compose(Map, LLPathOrMap).

mapping_table_funs(HiLoPairs) ->
    mapping_table_funs(HiLoPairs, [{J,I} || {I,J} <- HiLoPairs]).

mapping_table_funs(HiLoPairs, LoHiPairs) ->
    HToL = dict:from_list(HiLoPairs),
    LToH1 = dict:from_list(LoHiPairs),
    LToH = case dict:is_key(not_found, LToH1) of
               true -> LToH1;
               false -> dict:append(not_found, not_found, LToH1)
           end,
    UpFun =
        fun(LLVal) ->
                case dict:find(LLVal, LToH) of
                    {ok, HLVal} -> HLVal;
                    error -> ?LOGWARN("Unknown LLVal case", LLVal), LLVal
                end
        end,
    DnFun =
        fun(HLVal) ->
                case dict:find(HLVal, HToL) of
                    {ok, LLVal} -> LLVal;
                    error -> ?LOGWARN("Unknown HLVal case", HLVal), HLVal
                end
        end,
    {UpFun, DnFun}.

%% @spec from_cases(LLPath | mappings(), [tuple()]) -> mappings()
%%
%% @doc Mapping from HL choice cases to LL leaf values.  Another variant of {@link
%% address_map/2}.  `CaseMapPairs' is a list of tuples HL case name / LL value; maps
%% operations `get_case' and `set_case' to `get_elem' and `set_elem' respectively, while
%% mapping the case names to values and vice versa.
from_cases(LLPathOrMap, CaseMapPairs) ->
    from_cases(LLPathOrMap, CaseMapPairs, [{J,I} || {I,J} <- CaseMapPairs]).

%% @spec from_cases(LLPath | mappings(), [tuple()], [tuple()]) -> mappings()
%%
%% @doc Variant of {@link from_cases/2} accepting separate lists of value pairs for the two
%% directions, cases to values (HL to LL), and values to cases (LL to HL).
from_cases(LLPathOrMap, CaseToValPairs, ValToCasePairs) ->
    TopMap = #mappings{fopmap=fun(Tctx,set_case,Path,'$none',Mappings) ->
                                      {Tctx,delete,Path,none,Mappings};
                                 (Tctx,set_case,Path,Arg,Mappings) ->
                                      {Tctx,set_elem,Path,Arg,Mappings};
                                 (Tctx,get_case,Path,_Arg,Mappings) ->
                                      {Tctx,get_elem,Path,none,Mappings}
                              end,
                       %% needs to be done separately to preserve composability
                       fdnval=ec_genet:value_fun(fun({_Choice,Case}) -> Case;
                                                    (Arg) -> Arg end)},
    compose(TopMap, address_map(LLPathOrMap, CaseToValPairs, ValToCasePairs)).

%% @spec choice_map(LLChoiceOrMap, EnumPairs) -> mappings()
%% LLChoiceOrMap = LLChoicePath | mappings()
%% EnumPairs = [tuple()]
%%
%% @doc Case choice mapping generator.  The second argument is a list of high/low case
%% names.  Can be composed (?).  See also {@link scale_factor/2}.
%%
%% @deprecated Use {@link address_map/2} instead.
choice_map(LLChoiceOrMap, HiLoCasePairs) ->
    address_map(LLChoiceOrMap, HiLoCasePairs).

%% @spec guard_by_value(LLPath | mappings(), LLPath, value()) -> mappings()
%%
%% @doc Adds existence check to the mapping (or path).  Adds a `fexists' check: for get operations the mapping
%% first checks whether the LL leaf has the value as provided, if not, immediately return
%% `not_found', otherwise proceed with normal operations.
guard_by_value(LLPathOrMap, LLLeaf, LLValue) ->
    compose(#mappings{fexists=fun(Tctx,_,_) ->
                                      case ec_genet:get_elem(Tctx, LLLeaf, not_found) of
                                          LLValue -> true;
                                          _ -> not_found
                                      end
                              end},
            LLPathOrMap).

%% @spec exists_constant(HLPath, false | true) -> mappings()
%%
%% @doc Never existing / always existing leaf (empty or not) or a presence container.
%% Depending on the second argument, the node always reports exists=false (or true,
%% respectively) and can never be created or assigned a value (or deleted, respectively).
%% Cannot be composed.
exists_constant(HLPath, true) ->
    #mappings{exists=fun(_,_,_) -> true end,
              create=fun(_,_,_) -> ok end,
              delete=fun(_,_,_) -> throw({error, "Node " ++ econfd:pp_kpath(HLPath) ++
                                              " cannot be deleted"}) end};
exists_constant(HLPath, false) ->
    #mappings{exists=fun(_,_,_) -> false end,
              create=fun(_,_,_) -> throw({error, "Node " ++ econfd:pp_kpath(HLPath) ++
                                              " cannot be created"}) end,
              set_elem=fun(_,_,_,_) -> throw({error, "Node " ++ econfd:pp_kpath(HLPath) ++
                                                  " cannot be created"}) end,
              delete=fun(_,_,_) -> ok end}.

%% @spec sentinel_hl(Path, value()) -> mappings()
%%
%% @doc Produce a mapping that deals with HL sentinel value.  The value coming from the
%% user is either propagated to LL, or converted to the delete operation, if it is the
%% sentinel value.  Similarly, value going from LL is kept intact, unless the LL leaf does
%% not exists, in which case the sentinel value is returned.  Note that the value needs to
%% have the right type, see {@link sentinel_hl/3}.  Currently not composable (will be
%% fixed).
sentinel_hl(LLPath, SentinelValue) ->
    #mappings{path=LLPath,
              set_elem=fun(Tctx,_,LLVal,_) when LLVal==SentinelValue ->
                               ec_genet:delete_if_exists(Tctx, LLPath);
                          (Tctx,_,LLVal,_) ->
                               ec_genet:set_elem(Tctx, LLPath, LLVal)
                       end,
              get_elem=fun(Tctx,_,_) ->
                               ec_genet:get_elem(Tctx, LLPath, SentinelValue)
                       end}.

%% @spec sentinel_hl(Path, Path | Map, value()) -> mappings()
%%
%% @doc Version of {@link sentinel_hl/2} that takes care of the sentinel value type.
sentinel_hl(HLPath, LLPath, SentinelValue) ->
    sentinel_hl(LLPath, ec_genet_server:convert_value(HLPath, SentinelValue)).

%% @spec sentinel_ll(Path | Map, value()) -> mappings()
%%
%% @doc Produce a mapping that deals with LL sentinel value.  If the HL leaf is deleted by
%% the user, the change is propagated as set to the sentinel value; and if the LL value is
%% the sentinel, it is reported as unset.  Note that the value needs to have the right
%% type, see {@link sentinel_ll/3}.  Currently composable only from the right.
sentinel_ll(LLPathOrMap, LLSentinelValue) ->
    Map = #mappings{fupval=fun(_,_,LLVal,_) when LLVal==LLSentinelValue -> not_found;
                              (_,_,LLVal,_) -> LLVal end,
                    fopmap=fun(Tctx,delete,Path,_,Mappings) ->
                                   % FIXME: this makes it non-composable from the left!
                                   {Tctx,set_elem,Path,LLSentinelValue,Mappings};
                              (Tctx,Op,Path,Arg,Mappings) ->
                                   {Tctx,Op,Path,Arg,Mappings} end},
    compose(Map, LLPathOrMap). % this is the not working composition...

%% @spec sentinel_ll(Path, Path | Map, value()) -> mappings()
%%
%% @doc Version of {@link sentinel_ll/2} that takes care of the sentinel value type.
sentinel_ll(LLPath, LLPathOrMap, SentinelValue) ->
    sentinel_ll(LLPathOrMap, ec_genet_server:convert_value(LLPath, SentinelValue)).

%% @spec to_existence(Path | Map, value(), value()) -> mappings()
%%
%% @doc Return a mapping that maps from boolean/two-valued leaf *to* an empty leaf (or a
%% presence container).  Note that the two high-level values need to have the right type, see {@link
%% to_existence/4}.  Can be composed from the right.
to_existence(LLPathOrMap, NonexistsHLVal, ExistsHLVal) ->
    Map = #mappings{fopmap=fun(Tctx,set_elem,Path,HLVal,Mappings) when HLVal==NonexistsHLVal ->
                                   {Tctx,delete,Path,none,Mappings};
                              (Tctx,set_elem,Path,HLVal,Mappings) when HLVal==ExistsHLVal ->
                                   {Tctx,create,Path,none,Mappings};
                              (Tctx,get_elem,Path,Arg,Mappings) ->
                                   {Tctx,exists,Path,Arg,Mappings};
                              (Tctx,Op,Path,LLVal,Mappings) ->
                                   {Tctx,Op,Path,LLVal,Mappings}
                           end,
                    fupval=fun(_,_,false,_) -> NonexistsHLVal;
                              (_,_,true,_) -> ExistsHLVal end},
    compose(Map, LLPathOrMap).

%% @spec to_existence(Path, Path | Map, value(), value()) -> mappings()
%%
%% @doc Variant of {@link to_existence/3} that converts the values to appropriate types,
%% if possible.
to_existence(HLPath, LLPathOrMap, NonexistsHLVal, ExistsHLVal) ->
    to_existence(LLPathOrMap,
                 ec_genet_server:convert_value(HLPath, NonexistsHLVal),
                 ec_genet_server:convert_value(HLPath, ExistsHLVal)).

%% @spec from_existence(Path | Map, value(), value()) -> mappings()
%%
%% @doc Return a mapping that maps *from* existence (i.e. from an empty leaf) to a boolean
%% or a two-valued leaf.  Note that the two values need to have the right type, see {@link
%% from_existence/4}.  Can be composed from the right.
from_existence(LLPathOrMap, NonexistsLLVal, ExistsLLVal) ->
    from_existence_(LLPathOrMap, NonexistsLLVal, ExistsLLVal, set_elem).

%% @spec from_existence_delete(Path | Map, value()) -> mappings()
%%
%% @doc Variant of {@link from_existence/3} which deletes the LL leaf if the HL leaf is
%% deleted.
from_existence_delete(LLPathOrMap, ExistsLLVal) ->
    from_existence_(LLPathOrMap, none, ExistsLLVal, delete).

from_existence_(LLPathOrMap, NonexistsLLVal, ExistsLLVal, DeleteOp) ->
    Map = #mappings{fopmap=fun(Tctx,exists,Path,_,Mappings) ->
                                   {Tctx,get_elem,Path,none,Mappings};
                              (Tctx,create,Path,_,Mappings) ->
                                   {Tctx,set_elem,Path,ExistsLLVal,Mappings};
                              (Tctx,delete,Path,_,Mappings) ->
                                   {Tctx,DeleteOp,Path,NonexistsLLVal,Mappings}
                           end,
                    fupval=fun(_,_,Enum,_) -> Enum==ExistsLLVal end},
    compose(Map, LLPathOrMap).

%% @spec from_existence(Path, Path | Map, value(), value()) -> mappings()
%%
%% @doc Variant of {@link from_existence/3} that converts the values to appropriate types.
%% The values must correspond to the LLPath, see {@link ec_genet_server:convert_value/2}.
%% LLPath is often the same as LLPathOrMap.
from_existence(LLPath, LLPathOrMap, NonexistsLLVal, ExistsLLVal) ->
    from_existence(LLPathOrMap,
                   ec_genet_server:convert_value(LLPath, NonexistsLLVal),
                   ec_genet_server:convert_value(LLPath, ExistsLLVal)).


%% @spec from_existence_delete(Path, Path | Map, value()) -> mappings()
%%
%% @doc Variant of {@link from_existence/4} which deletes the LL leaf if the HL leaf is
%% deleted.  See also {@link from_existence_delete/2}.
from_existence_delete(LLPath, LLPathOrMap, ExistsLLVal) ->
    from_existence_delete(LLPathOrMap, ec_genet_server:convert_value(LLPath, ExistsLLVal)).

%% @spec pre_hook(Path | Map, Map) -> mappings()
%%
%% @doc Generate a mapping that embeds the provided mapping (or mapping with the path),
%% but invokes given set of hooks for the operations in `HookMap' before the operation is
%% performed on `LLPathOrMap'.  `HookMap' is a mappings that defines those DP-API
%% functions (`get_elem', `set_elem' and the like) that should be called before this
%% operation is to be performed.
pre_hook(LLPathOrMap, HookMap) ->
    #mappings{nested=[hook_map(HookMap),
                      compose(#mappings{}, LLPathOrMap)],
              fdnval=ec_genet:value_fun(fun(Val) -> [Val, Val] end),
              fupval=ec_genet:value_fun(fun([_,Val]) -> Val end)}.

%% @spec post_hook(Path | Map, Map) -> mappings()
%%
%% @doc Counterpart of {@link pre_hook/2}.
post_hook(LLPathOrMap, HookMap) ->
    #mappings{nested=[compose(#mappings{}, LLPathOrMap),
                      hook_map(HookMap)],
              fdnval=ec_genet:value_fun(fun(Val) -> [Val, Val] end),
              fupval=ec_genet:value_fun(fun([Val,_]) -> Val end)}.

%% @spec address_switch(Path | Map, Path | Map) -> mappings()
%%
%% @doc Mapping a single leaf to a pair of leaves, e.g. in a choice.
%% The value for `set' is supposed to be pair `{X, Val}' where `X' is
%% either true or false to indicate the first or the second of the
%% leaves; return value of `get' is similarly `{X, Val}' to indicate
%% from which of the two paths the value was read, or `not_found' if
%% none of the LL leaves exists.  If the mechanism for determining the
%% correct address (mapping) cannot be based only on the value, try
%% using {@link mappings_switch/3} instead.
address_switch(PathOrMapTrue, PathOrMapFalse) ->
    #mappings{nested=[], % will be rewritten
              fupval=fun(_,_,[not_found,not_found],_) -> not_found;
                        (_,_,[not_found,Val],_) -> {false,Val};
                        (_,_,[Val,_],_) -> {true,Val}
                     end,
              fopmap=fun(Tctx,set_elem,Path,{X,Val},Mappings) ->
                             PathOrMap = if X -> PathOrMapTrue; true -> PathOrMapFalse end,
                             {Tctx,set_elem,Path,[Val],Mappings#mappings{nested=[path_to_mappings(PathOrMap)]}};
                        (Tctx,Op,Path,Arg,Mappings) ->
                             {Tctx,Op,Path,Arg,Mappings#mappings{nested=[path_to_mappings(PathOrMapTrue),
                                                                         path_to_mappings(PathOrMapFalse)]}}
                     end}.

%% @spec existence_to_list_instance(Path) -> mappings()
%%
%% @doc A mappings that translates a presence container to instance existence.  It ignores
%% `create' operation, it is supposed to be handled by other means.
existence_to_list_instance(LLList) ->
    #mappings{path=LLList,
              create=fun(_,_,_) -> ok end,
              exists=fun(Tctx,_,_) -> ec_genet:num_instances(Tctx, LLList) > 0 end}.

%% @spec leaf_to_list_instance(Path, atom()) -> mappings()
%%
%% @doc Mapping translating operations on a leaf to operations on a single-instance list.
%% When the HL leaf value is retrieved, it looks at the instance key value, when it is
%% set, mapping renames the instance, if it exists, creates it otherwise.
leaf_to_list_instance(LLList, KeyName) ->
    #mappings{get_elem=fun(Tctx,_,_) -> {As} = ec_genet:get_first_keyset(Tctx, LLList), As end,
              set_elem=fun(Tctx,_,As,_) -> %% If an instance already exists, it should be moved/renamed
                               CAs = ec_genet_server:convert_value([KeyName|LLList], As),
                               case ec_genet:get_first_keyset(Tctx, LLList) of
                                   not_found -> ec_genet:create(Tctx, [{CAs}|LLList]);
                                   {OldAs}   -> ec_genet:move(Tctx, [{OldAs}|LLList], {CAs})
                               end
                       end}.

%% @spec node_to_list_instance_subnode(Path | Map, Path) -> mappings()
%%
%% @doc Mapping a node to a LL list subnode.  The LL list is used as a `fexists' check,
%% and the full path is filled in with the appropriate LL list instance key.
node_to_list_instance_subnode(LLPathOrMap, LLList) ->
    Mappings = #mappings{fexists=fun(Tctx,_,_) -> ec_genet:get_first_keyset(Tctx,LLList) end,
                         fdnkeys=fun(Tctx,_,_) -> [ec_genet:get_first_keyset(Tctx,LLList)] end},
    compose(Mappings, LLPathOrMap).

hook_map(HookMap) ->
    HookMap#mappings{path=[],
                     fopmap=fun(Tctx,_,Path,Arg,Mappings) ->
                                    {Tctx,nop,Path,Arg,Mappings}
                            end}.

path_to_mappings(PathOrMap) ->
    compose(#mappings{}, PathOrMap).

%% @spec leaf_list_map(Path | Map, integer(), integer()) -> mappings()
%%
%% @doc Automatic conversion of leaf-lists, provided that the underlying type is one of
%% the integer types.
leaf_list_map(LLPathOrMap, Type, Type) ->
    path_to_mappings(LLPathOrMap);
leaf_list_map(LLPathOrMap, HLType, LLType) ->
    compose(#mappings{
               fupval=ec_genet:value_fun(fun(?CONFD_LIST(List)) ->
                                                 ?CONFD_LIST([to_int(from_int(I, LLType), HLType) ||
                                                                 I <- List])
                                         end),
               fdnval=ec_genet:value_fun(fun(?CONFD_LIST(List)) ->
                                                 ?CONFD_LIST([to_int(from_int(I, HLType), LLType) ||
                                                                 I <- List])
                                         end)},
            LLPathOrMap).

to_int(Int, ?C_INT32) ->
    Int;
to_int(Int, Type) ->
    {Type, Int}.

from_int(Int, ?C_INT32) ->
    Int;
from_int({Type, Int}, Type) ->
    Int.

%% @spec compose(mappings(), mappings() | Path) -> mappings()
%%
%% @doc Compose the two mappings (or mappings and a path) such that the first operates "on
%% top" of the second.  For instance if both declare fupval, the second mappings' fupval
%% is applied first, then the first one; for fdnval the application is the other way
%% round.
compose(Map, Path) when is_list(Path) ->
    Map#mappings{path=Path};
compose(Map1, Map2=#mappings{}) ->
    UpVal = compose_fns(Map1#mappings.fupval, Map2#mappings.fupval),
    UpKeys = compose_fns(Map1#mappings.fupkeys, Map2#mappings.fupkeys),
    DnVal = compose_fns(Map2#mappings.fdnval, Map1#mappings.fdnval),
    (merge_records(Map1, Map2))#mappings{fupkeys=UpKeys, fupval=UpVal, fdnval=DnVal}.

compose_fns(none, F) -> F;
compose_fns(F, none) -> F;
compose_fns(F1, F2) ->
    fun (Tctx, Op, Val, Extra) ->
            %% TODO special support for Extra needed?
            NVal = F2(Tctx, Op, Val, Extra),
            F1(Tctx, Op, NVal, Extra)
    end.


%% @spec merge_mappings(mappings(), mappings()) -> mappings()
%%
%% @doc Merge two mappings as for inheriting.  Basicaly just add all parent's fields that
%% are not present in the child record, only fdnpath needs bit more complex processing.
merge_mappings(ChildRecord, ParentRecord) ->
    NChildRecord = ChildRecord#mappings{fdnpath=merge_fdnpath(ChildRecord#mappings.fdnpath,
                                                              ChildRecord#mappings.relpath,
                                                              ParentRecord#mappings.fdnpath)},
    merge_records(NChildRecord, ParentRecord).

merge_fdnpath(X, RelPath, _) when RelPath == none; RelPath == [] -> X;
merge_fdnpath(X, _, none) -> X;
merge_fdnpath(X, _, _) when X /= none -> X;
merge_fdnpath(none, L, X) ->
    fun(Tctx, LLPath, Extra) ->
            L ++ X(Tctx, lists:nthtail(length(L), LLPath), Extra)
    end.

merge_records(ChildRecord, ParentRecord) ->
    list_to_tuple(
      lists:zipwith(fun(Child,Parent) ->
                            case Child of
                                none -> Parent;
                                _ -> Child
                            end
                    end,
                    tuple_to_list(ChildRecord),
                    tuple_to_list(ParentRecord))).

%% @spec leaf_list_to_leaf(Path | mappings()) -> mappings()
%%
%% @doc Genereate a mapping that maps a leaf-list to a leaf.  The mapping "pretends" to
%% accept a leaf-list, but verifies that it contains exactly one element (for set).
leaf_list_to_leaf(LLPathOrMap) ->
    Map = #mappings{
             fupval=ec_genet:value_fun(fun(Val) -> [Val] end),
             fdnval=ec_genet:value_fun(fun([Val]) -> Val;
                                          (_) -> throw({error, "Only one value supported"}) end)
            },
    compose(Map, LLPathOrMap).

%% @doc Switching between two variant mappings.  Similar to
%% `address_switch', but in this case the selection of which path or
%% mapping should be used is a "switching function", which is invoked
%% before every operation with the current transaction context and HL
%% path.
-spec mappings_switch(path_or_map(), path_or_map(),
                      fun((#confd_trans_ctx{}, HLPath::path()) -> true | false)) ->
                               #mappings{}.
mappings_switch(PathOrMap1, PathOrMap2, SwitchFun) ->
    Map1 = path_to_mappings(PathOrMap1),
    Map2 = path_to_mappings(PathOrMap2),
    #mappings{
       fopmap=fun(Tctx, Op, HLPath, Arg, _) ->
                      case SwitchFun(Tctx, HLPath) of
                          true -> {Tctx, Op, HLPath, [Arg], #mappings{nested=[Map1]}};
                          false -> {Tctx, Op, HLPath, [Arg], #mappings{nested=[Map2]}}
                      end
              end,
       fupval=ec_genet:value_fun(fun([X]) -> X end),
       fupkeys=ec_genet:value_fun(fun([X]) -> X end),
       nested=[Map1, Map2]
      }.

%% @doc Compose two or more list mappings to one.  Equivalent to
%% `composite_list(PathOrMaps, [])'.
-spec composite_list(list(path_or_map())) -> #mappings{}.
composite_list(PathOrMaps) ->
    composite_list(PathOrMaps, []).

%% @doc Compose two or more list mappings to one.  The parameter
%% PathOrMaps is a list of simple paths or mappings instances, Options
%% is a list of options; currently only `mode' with values `union' or
%% `each' is supported, defaults to `union'.
%%
%% The lists are all used for `get_next' queries, the keys returned
%% are sorted and returned one by one.  If two of the mappings return
%% the same key and the `mode' option is `union', the key is used only
%% once.
-spec composite_list(list(path_or_map()), list({atom(),any()})) -> #mappings{}.
composite_list(PathOrMaps, Options) ->
    #mappings{
       fopmap=fun(Tctx, get_next, _Path, Arg, _Map) ->
                      {Args, Map} = composite_list_opmap(PathOrMaps, Options, Arg),
                      {Tctx, get_next, [], Args, Map}
              end,
       fupkeys=fun(_,_,[RetVal],_) -> RetVal end}.

composite_list_opmap(PathOrMaps, _Options, -1) ->
    Maps = [path_to_mappings(PM) || PM <- PathOrMaps],
    {-1, composite_list_build_mappings(Maps, {Maps, []})};
composite_list_opmap(_PMs, Options, State) ->
    {Args, Maps, Rest} =
        case proplists:get_value(mode, Options, union) of
            union ->
                composite_list_advance_union(State);
            each ->
                composite_list_advance_one(State)
        end,
    {[Args], composite_list_build_mappings(Maps, {Maps,Rest})}.

%% @doc Mark all mappings that produced the last-used key as mappings
%% that need to be "advanced", i.e. should be passed to `get_next'
%% processing.
composite_list_advance_union([{Key,Cursor,Map},NS={Key,_,_}|Rest]) ->
    {Args, Maps, RestMaps} = composite_list_advance_union([NS|Rest]),
    {[Cursor|Args], [Map|Maps], RestMaps};
composite_list_advance_union(State) ->
    composite_list_advance_one(State).

composite_list_advance_one([{_,Cursor,Map} | Rest]) ->
    {[Cursor], [Map], Rest};
composite_list_advance_one([]) ->
    {[], [], []}.

%% @doc Build nested-in-nested mappings.  The two levels are necessary
%% because `extra' is generated dynamically and would be ignored in
%% the top-level mappings.
composite_list_build_mappings(Maps, Extra) ->
    #mappings{nested=[#mappings{nested=Maps, extra=Extra, fupkeys=fun composite_list_upkeys/4}]}.

%% @doc Process values returned by the composed mappings.  Pair
%% returned {keyset,cursor} pairs with all mappings that were advanced
%% filtering out all terminated mappings, concatenate it with the
%% remaining mappings and sort.
composite_list_upkeys(_Tctx, _Op, RetKeys, {Advanced, Rest}) ->
    ZipReturn = [{Key, Cursor, Map} ||
                    {{Key, Cursor}, Map} <- lists:zip(RetKeys, Advanced),
                    {Key, Cursor} /= {false, undefined}],
    Sorted = lists:sort(fun({Key1,_,_}, {Key2,_,_}) -> Key1 < Key2 end,
                        ZipReturn ++ Rest),
    case Sorted of
        [] ->
            {false, undefined};
        [{Key,_,_}|_] ->
            {Key, Sorted}
    end.
