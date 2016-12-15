-module(ec_map_example_server).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Genet example model <-> Genet example device model
%% Genet transformation module
%%
%% Maps
%% module trafo-example {
%%  namespace "http://cisco.com/ns/genet/examples/transforms";
%% to 
%% module trafo-example-device {
%%  namespace "http://cisco.com/ns/genet/examples/transforms/device";
%%
%% This module serves as an example catalog of common mappings
%%
%% (C) 2015 Cisco Systems/Tail-f Systems. All rights reserved.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-behaviour(gen_server).

%% HLROOT and LLROOT define where the mappings are going from and to
%% included headers generated from the YANG sources
-define(HLROOT, ['http://cisco.com/ns/genet/examples/transforms'|'example-domain']).
-include("trafo-example.hrl").

-define(LLROOT, ['http://cisco.com/ns/genet/examples/transforms/device'|'example-device']).
-include("trafo-example-device.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("econfd/include/econfd.hrl").
-include_lib("econfd/include/econfd_errors.hrl").
-include("ec_genet.hrl").
-include("debug_macros.hrl").
-include("mapgens_macros.hrl").

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

%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true), % Triggers call to terminate/2
    ok = ec_genet:start([?HLROOT], fun get_mappings/1),
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
    ok = ec_genet:stop([?HLROOT]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mappings(HLPath) when is_list(HLPath) ->
    case HLPath of

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Direct Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        [a,'leaf-string',direct,?HLROOT] ->
            %%X $HLROOT/direct/leaf-string/a <-> $LLROOT/direct/leaf-string/k;
            #mappings{path=[k,'leaf-string',direct,?LLROOT]};
        [a,'leaf-uint',direct,?HLROOT] ->
            #mappings{path=[k,'leaf-uint',direct,?LLROOT]};
        [a,'leaf-ip',direct,?HLROOT] ->
            #mappings{path=[k,'leaf-ip',direct,?LLROOT]};
        [a,'leaf-empty',direct,?HLROOT] ->
            #mappings{path=[k,'leaf-empty',direct,?LLROOT]};
        %% Container [container,direct,?HLROOT] needs no mapping
        ['container-presence',direct,?HLROOT] ->
            #mappings{path=['container-presence',direct,?LLROOT]};

        %% Augment
        %% [abc,['6768776'|xyz],['urn:ietf-syslog:something'|syslog]] ->
        %%      #mappings{}

        %% Choice mappoing
        [[a],choice,direct,?HLROOT] ->
            ec_genet_mapgens:address_map([[k],choice,direct,?LLROOT],
                                              [{'b','n'},
                                               {'g','q'},
                                               {'h','r'}]);
        %% Case [[b,a],choice,direct,?HLROOT] needs no mapping
        [e,choice,direct,?HLROOT] ->
            #mappings{path=[o,choice,direct,?LLROOT]};
        [f,choice,direct,?HLROOT] ->
            #mappings{path=[p,choice,direct,?LLROOT]};
        [g,choice,direct,?HLROOT] ->
            #mappings{path=[q,choice,direct,?LLROOT]};
        %% Container [h,choice,direct,?HLROOT] needs no mapping
        [i,h,choice,direct,?HLROOT] ->
            #mappings{path=[s,r,choice,direct,?LLROOT]};
        [j,h,choice,direct,?HLROOT] ->
            #mappings{path=[t,r,choice,direct,?LLROOT]};

        %% Choice-in-choice mappoing
        [[a],'choice-in-choice',direct,?HLROOT] ->
            ec_genet_mapgens:address_map([[k],'choice-in-choice',direct,?LLROOT],
                                              [{b,l}, {h,r}]);
        %% Case [[b,a],'choice-in-choice',direct,?HLROOT] needs no mapping
        [[c,b,a],'choice-in-choice',direct,?HLROOT] ->
            ec_genet_mapgens:address_map([[m,l,k],'choice-in-choice',direct,?LLROOT],
                                              [{d,n}, {g,q}]);
        %% Case [[d,c,b,a],'choice-in-choice',direct,?HLROOT] needs no mapping
        [e,'choice-in-choice',direct,?HLROOT] ->
            #mappings{path=[o,'choice-in-choice',direct,?LLROOT]};
        [f,'choice-in-choice',direct,?HLROOT] ->
            #mappings{path=[p,'choice-in-choice',direct,?LLROOT]};
        [g,'choice-in-choice',direct,?HLROOT] ->
            #mappings{path=[q,'choice-in-choice',direct,?LLROOT]};
        %% Container [h,'choice-in-choice',direct,?HLROOT] needs no mapping
        [i,h,'choice-in-choice',direct,?HLROOT] ->
            #mappings{path=[s,r,'choice-in-choice',direct,?LLROOT]};
        [j,h,'choice-in-choice',direct,?HLROOT] ->
            #mappings{path=[t,r,'choice-in-choice',direct,?LLROOT]};

        %% a leaf-list is mapped pretty much the same as a leaf (in simple cases)
        [a,'leaf-list',direct,?HLROOT] ->
            #mappings{path=[k,'leaf-list',direct,?LLROOT]};

        %% for a list, we need to map the list, list instance, the first key leaf, and the
        %% ordinary leaves or containers, if any
        [a,list,direct,?HLROOT] ->
            #mappings{path=[k,list,direct,?LLROOT]};
        %% for the instance we can grab the key ({X} in the code) and use it as a key in
        %% the LL path
        [{X},a,list,direct,?HLROOT] ->
            #mappings{path=[{X},k,list,direct,?LLROOT]};
        [b,{X},a,list,direct,?HLROOT] ->
            #mappings{path=[l,{X},k,list,direct,?LLROOT]};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Types Example Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %% all the mappings below are direct and do not need any mapping; we are
        %% implementing fdnval functions to examplify how to handle the types

        %% Simple types do not need special handling and can be matched or creating using
        %% the CONFD_XXX macros
        [a,int32,integers,types,?HLROOT] ->
            #mappings{path=[k,int32,integers,types,?LLROOT],
                      fdnval=fun(_,_,?CONFD_INT32(HLVal),_) ->
                                     ?LOGMSG("Received integer", HLVal),
                                     ?CONFD_INT32(HLVal)
                             end};
        [a,uint32,integers,types,?HLROOT] ->
            #mappings{path=[k,uint32,integers,types,?LLROOT],
                      fdnval=fun(_,_,?CONFD_UINT32(HLVal),_) ->
                                     ?LOGMSG("Received unigned integer", HLVal),
                                     ?CONFD_UINT32(HLVal)
                             end};
        [a,uint8,integers,types,?HLROOT] ->
            #mappings{path=[k,uint8,integers,types,?LLROOT],
                      fdnval=fun(_,_,?CONFD_UINT8(HLVal),_) ->
                                     ?LOGMSG("Received unsigned 8 bit integer", HLVal),
                                     ?CONFD_UINT8(HLVal)
                             end};
        %% Strings are represented by "Binaries"; for converting between Binaries and
        %% Erlang strings (lists of characters) we can use binary_to_list and
        %% list_to_binary
        [a,string,types,?HLROOT] ->
            #mappings{path=[k,string,types,?LLROOT],
                      fdnval=fun(_,_,?CONFD_BUF(HLVal),_) ->
                                     ?LOGMSG("Received string", binary_to_list(HLVal)),
                                     ?CONFD_BUF(HLVal)
                             end};
        %% Union type values are passed as is, so if we need to know which type we have,
        %% we can use matching.
        [a,union,types,?HLROOT] ->
            #mappings{path=[k,union,types,?LLROOT],
                      fdnval=fun(_,_,HLVal,_) ->
                                     case HLVal of
                                         %% some of the types (e.g. int32 or string) are
                                         %% represented as "untagged values",
                                         %% i.e. matching only against the ?CONFD_INT32
                                         %% may not be enough, we also need to verify the
                                         %% type of the argument
                                         ?CONFD_UINT8(U8Val) ->
                                             ?LOGMSG("Received uint8 of the union", U8Val),
                                             ?CONFD_UINT8(U8Val);
                                         ?CONFD_INT32(I32Val) when is_integer(I32Val) ->
                                             ?LOGMSG("Received int32 of the union", I32Val),
                                             ?CONFD_INT32(I32Val);
                                         ?CONFD_BUF(StrBin) when is_binary(StrBin) ->
                                             ?LOGMSG("Received string of the union", binary_to_list(StrBin)),
                                             ?CONFD_BUF(StrBin);
                                         ?CONFD_BOOL(Bool) when is_atom(Bool) ->
                                             %% either atom 'true' or 'false'
                                             ?LOGMSG("Received boolean of the union", Bool),
                                             ?CONFD_BOOL(Bool)
                                     end
                             end};
        [a,'ip-address',types,?HLROOT] ->
            #mappings{path=[k,'ip-address',types,?LLROOT],
                      fdnval=fun(_,_,HLVal,_) ->
                                     case HLVal of
                                         ?CONFD_IPV4(Ipv4) when tuple_size(Ipv4) == 4 ->
                                             %% IPv4 address itself is represented as a 4-tuple of address bytes
                                             ?LOGMSG("Received IPv4 address", Ipv4),
                                             ?CONFD_IPV4(Ipv4);
                                         ?CONFD_IPV4(Ipv6) when tuple_size(Ipv6) == 8 ->
                                             %% IPv6 address is represented as a 8-tuple of address words
                                             ?LOGMSG("Received IPv6 address", Ipv6),
                                             ?CONFD_IPV6(Ipv6)
                                     end
                             end};
        [a,'ipv6-prefix',types,?HLROOT] ->
            #mappings{path=[k,'ipv6-prefix',types,?LLROOT],
                      fdnval=fun(_,_,?CONFD_IPV6PREFIX({Addr,PrefLen}),_) ->
                                     ?LOGMSG("Received IPv6 prefix", Addr, PrefLen),
                                     ?CONFD_IPV6PREFIX({Addr,PrefLen})
                             end};

        [a,'leaf-list',types,?HLROOT] ->
            #mappings{path=[k,'leaf-list',types,?LLROOT],
                      fdnval=fun(_,_,?CONFD_LIST(List),_) ->
                                     Length = length(List),
                                     First = case List of
                                                 [?CONFD_UINT8(X)|_Rest] -> X;
                                                 [] -> "(empty)"
                                             end,
                                     ?LOGMSG("Received a leaf-list instance, checking the first element",
                                             Length, First),
                                     ?CONFD_LIST(List)
                             end};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Value Domain Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        [a,constant,value,?HLROOT] ->
            ec_genet_mapgens:constant(HLPath, 4711); % should be CONFD_UINT32; auto type-converts deals with this
        [a,expanding,range,value,?HLROOT] ->
            #mappings{path=[k,expanding,range,value,?LLROOT],
                      %% down-value is done fine; we need to deal with the "invalid" LL value
                      fupval=ec_genet:int_value_fun(fun(3) ->
                                                            ?LOGMSG("Lying about LL value"), 2;
                                                       (LLVal) -> LLVal
                                                    end)};
        [a,contracting,range,value,?HLROOT] ->
            #mappings{path=[k,contracting,range,value,?LLROOT],
                      %% up-value is done fine; we only need to throw an error for the invalid HL value
                      fdnval=ec_genet:int_value_fun(fun(3) -> throw({error, "Value 3 cannot be supported"});
                                                       (HLVal) -> HLVal
                                                    end)};


        [a,expanding,scaling,value,?HLROOT] ->
            ec_genet_mapgens:scale_factor([k,expanding,scaling,value,?LLROOT], 2.55);
        [a,contracting,scaling,value,?HLROOT] ->
            ec_genet_mapgens:contract_factor([k,contracting,scaling,value,?LLROOT], 2.55);
        [a,precision,value,?HLROOT] ->
            #mappings{path=[k,precision,value,?LLROOT],
                      fdnval=ec_genet:value_fun(fun(?CONFD_DECIMAL64({V,2})) -> ?CONFD_UINT8(V div 100) end),
                      fupval=ec_genet:value_fun(fun(?CONFD_UINT8(V)) -> ?CONFD_DECIMAL64({V*100,2}) end)};
        [a,offset,value,?HLROOT] ->
            ec_genet_mapgens:offset_value([k,offset,value,?LLROOT], 1);
        [a,composition,value,?HLROOT] ->
            ec_genet_mapgens:offset_value(
              ec_genet_mapgens:scale_factor([k,composition,value,?LLROOT], 1.275),
              100); %% when mapping down: first offset the value, then scale

        [a,integers,types,value,?HLROOT] ->
            #mappings{path=[k,integers,types,value,?LLROOT]};
        [a,lists,types,value,?HLROOT] ->
            #mappings{path=[k,lists,types,value,?LLROOT]};
        [{X},a,lists,types,value,?HLROOT] ->
            #mappings{path=[{X},k,lists,types,value,?LLROOT]};
        [b,{X},a,lists,types,value,?HLROOT] ->
            #mappings{path=[l,{X},k,lists,types,value,?LLROOT]};

        [a,'leaf-list',types,value,?HLROOT] ->
            ec_genet_mapgens:leaf_list_map([k,'leaf-list',types,value,?LLROOT], ?C_INT8, ?C_UINT8);

        [a,'string-to-binary',representation,value,?HLROOT] ->
            #mappings{path=[k,'string-to-binary',representation,value,?LLROOT],
                      %% we need to convert the value and change the type tag
                      fupval=ec_genet:value_fun(fun(?CONFD_BINARY(LLVal)) -> ?CONFD_BUF(binary2hexstring(LLVal)) end),
                      fdnval=ec_genet:value_fun(fun(?CONFD_BUF(HLVal)) -> ?CONFD_BINARY(hexstring2binary(HLVal)) end)};
        %% No need to convert anything, done automatically
        [a,'integer-to-string',representation,value,?HLROOT] ->
            #mappings{path=[k,'integer-to-string',representation,value,?LLROOT]};

        [a,identityref,representation,value,?HLROOT] ->
            #mappings{path=[k,identityref,representation,value,?LLROOT],
                      %% converting from string to identityref (and back)
                      fupval=ec_genet:value_fun(fun(?CONFD_BUF(LLVal)) ->
                                                        %% first, find the appropriate identity id
                                                        %% (using macros from a generated .hrl file)
                                                        IdentityId = case LLVal of
                                                                         <<"id1">> -> ?ex_example_identity_id1;
                                                                         <<"id2">> -> ?ex_example_identity_id2;
                                                                         <<"id3">> -> ?ex_example_identity_id3
                                                                     end,
                                                        %% now return the representation of the identity
                                                        %% (including the namespace)
                                                        ?CONFD_IDENTITYREF({?ex__ns, IdentityId})
                                                end),
                      fdnval=ec_genet:value_fun(fun(?CONFD_IDENTITYREF({?ex__ns, IdentityId})) ->
                                                        %% similarly, extract the id and format the
                                                        %% corresponding string representation
                                                        Name = case IdentityId of
                                                                   %% note that the return value "id1" is not
                                                                   %% valid, but this string value is automatically
                                                                   %% converted to a binary
                                                                   ?ex_example_identity_id1 -> "id1";
                                                                   ?ex_example_identity_id2 -> <<"id2">>;
                                                                   ?ex_example_identity_id3 -> <<"id3">>
                                                               end,
                                                        ?CONFD_BUF(Name)
                                                end)};

        [a,enumerations,value,?HLROOT] ->
            ec_genet_mapgens:enumeration_map([k,enumerations,value,?LLROOT],
                                             [{?ex_alert, ?exdev_alert},
                                              {?ex_debug, ?exdev_debugging},
                                              {?ex_info, ?exdev_info},
                                              {?ex_trace, ?exdev_trace}]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Structure Domain Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %% Mapping one leaf to multiple leafs amounts to copying the value to the two
        %% nested maps on the way down to LL, and taking one of the values on the way up.
        [a,'one-to-multiple-copies',leafs,structure,?HLROOT] ->
            LLBase = ['one-to-multiple-copies',leafs,structure,?LLROOT],
            #mappings{nested=[#mappings{path=[k|LLBase]},
                              #mappings{path=[l|LLBase]}],
                      fdnval=ec_genet:value_fun(fun(Val) -> [Val,Val] end),
                      fupval=ec_genet:value_fun(fun([Val,_]) -> Val end)};


        %% Similar to the above, with somewhat more complex computation of values in both
        %% directions.
        [a,'one-to-multiple-parts',leafs,structure,?HLROOT] ->
            LLBase = ['one-to-multiple-parts',leafs,structure,?LLROOT],
            #mappings{nested=[#mappings{path=[k|LLBase]},
                              #mappings{path=[l|LLBase]}],
                      fdnval=ec_genet:value_fun(fun(Prefix) ->
                                                        {Addr,Mask} = ec_genet:prefix_to_addrmask(Prefix),
                                                        [Addr,Mask]
                                                end),
                      fupval=ec_genet:value_fun(fun([X,Y]) when X == not_found; Y == not_found -> not_found;
                                                   ([Addr, Mask]) -> ec_genet:addrmask_to_prefix(Addr,Mask)
                                                end)};

        %% Here we need to map a HL leaf to a "half" of a LL leaf; extracting the HL value
        %% from the LL value is straightforward, the tricky part is the other way round -
        %% we need to get the LL value so that we can preserve the other half of it (and
        %% think it up if it does not exist).
        [a,'many-to-one',leafs,structure,?HLROOT] ->
            LLPath = [k,'many-to-one',leafs,structure,?LLROOT],
            #mappings{path=LLPath,
                      fupval=ec_genet:value_fun(fun(V) ->
                                                        list_to_integer(hd(string:tokens(binary_to_list(V), ":")))
                                                end),
                      fdnval=fun(Tctx,_,?CONFD_UINT32(V),_) ->
                                     K = ec_genet:get_elem(Tctx, LLPath, <<"1:1">>),
                                     [_,B] = string:tokens(binary_to_list(K), ":"),
                                     list_to_binary(integer_to_list(V) ++ ":" ++ B)
                             end};
        [b,'many-to-one',leafs,structure,?HLROOT] ->
            LLPath = [k,'many-to-one',leafs,structure,?LLROOT],
            #mappings{path=LLPath,
                      fupval=ec_genet:value_fun(fun(V) ->
                                                        list_to_integer(hd(tl(string:tokens(binary_to_list(V), ":"))))
                                                end),
                      fdnval=fun(Tctx,_,?CONFD_UINT32(V),_) ->
                                     K = ec_genet:get_elem(Tctx, LLPath, <<"1:1">>),
                                     [A,_] = string:tokens(binary_to_list(K), ":"),
                                     list_to_binary(A ++ ":" ++ integer_to_list(V))
                             end};

        %% Here we decided that the HL list should return a list
        %% entry key each time a particular LL list entry exists
        %% in either list or both -- union mode.
        %% Say the LL list k contains entries 1, 2, 5 and the
        %% LL list l contains entries 1, 5, 7. We will report
        %% the HL list to contain keys 1, 2, 5, 7 -- union mode
        %% Had we chosen intersection mode, the HL list would
        %% would have contained keys 1, 5 only.
        %% In 'each' mode, the HL list would have contained
        %% 1, 1, 2, 5, 5, 7. Repeated keys are illegal so we
        %% ought not to choose this mode unless we modify the
        %% keys somehow to make them unique.
        [a,'split-by-cols',lists,structure,?HLROOT] ->
            #mappings{get_next=fun ec_genet:composite_list_get_next/4,
                      extra=[{composite_list_get_next_mode,union},
                             {composite_list_get_next,
                              [[k,'split-by-cols',lists,structure,?LLROOT],
                               [l,'split-by-cols',lists,structure,?LLROOT]]}]
                     };
        %% Here we chose to map the key to both LL list k and list l
        %% So creation, reading and all will be directed to both
        %% lists. The fupval and fdnval functions are used to combine
        %% (fupval) and duplicate (fdnval) the value so that each
        %% creation/... get a value and get_elem/... values are
        %% combined.
        [{KC},a,'split-by-cols',lists,structure,?HLROOT] ->
            #mappings{nested=[#mappings{path=[{KC},k,'split-by-cols',lists,structure,?LLROOT]},
                              #mappings{path=[{KC},l,'split-by-cols',lists,structure,?LLROOT]}],
                      fupval=fun(_,_,LLVals,_) -> ec_genet:join_pick_not_equals(not_found,LLVals) end,
                      fdnval=fun(_,_,HLVal,_) -> ec_genet:dup(2,HLVal) end
                     };
        [c,{KC},a,'split-by-cols',lists,structure,?HLROOT] ->            
            #mappings{inherit=tl(HLPath),
                      nested=[#mappings{path=[m,{KC},k,'split-by-cols',lists,structure,?LLROOT]},
                              #mappings{path=[m,{KC},l,'split-by-cols',lists,structure,?LLROOT]}]
                     };
        [d,{KC},a,'split-by-cols',lists,structure,?HLROOT] ->
            #mappings{path=[n,{KC},k,'split-by-cols',lists,structure,?LLROOT]};
        [e,{KC},a,'split-by-cols',lists,structure,?HLROOT] ->
            #mappings{path=[o,{KC},l,'split-by-cols',lists,structure,?LLROOT]};

        %% The set of list instances is a union of instances of the LL list; for
        %% creation and deletion we address both LL lists, but when the
        %% discriminator leaf is set, we need to delete/create entries in the LL
        %% lists and copy the contents
        [a,union,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{nested=[#mappings{get_next=fun ec_genet:composite_list_get_next/4,
                                        extra=[{composite_list_get_next_mode,union},
                                               {composite_list_get_next,
                                                [[ipv4,union,'split-by-rows',lists,structure,?LLROOT],
                                                 [ipv6,union,'split-by-rows',lists,structure,?LLROOT]]}]
                                       }],
                      fupkeys=fun(_,_,[{false,undefined}],_) ->
                                      {false,undefined};
                                 (_,_,[{Key,C}],_) ->
                                      {Key,C}
                              end
                     };
        [{KB},a,union,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{nested=[#mappings{path=[{KB},ipv4,union,'split-by-rows',lists,structure,?LLROOT]},
                              #mappings{path=[{KB},ipv6,union,'split-by-rows',lists,structure,?LLROOT]}],
                      fdnval=fun(_,_,HLVal,_) -> ec_genet:dup(2,HLVal) end
                     };
        [b,{KB},a,union,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{nested=[#mappings{path=[m,{KB},ipv4,union,'split-by-rows',lists,structure,?LLROOT]},
                              #mappings{path=[m,{KB},ipv6,union,'split-by-rows',lists,structure,?LLROOT]}],
                      fupval=fun(_,_,[not_found,not_found],_) -> not_found;
                                (_,_,_,_) -> KB end
                     };
        [c,{KB},a,union,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      nested=[#mappings{path=[m,{KB},ipv4,union,'split-by-rows',lists,structure,?LLROOT]},
                              #mappings{path=[m,{KB},ipv6,union,'split-by-rows',lists,structure,?LLROOT]}],
                      fupval=fun(_,_,[not_found,not_found],_) -> not_found;
                                (_,_,[_,not_found],_) -> ?ex_ipv4;
                                (_,_,[not_found,_],_) -> ?ex_ipv6;
                                (_,_,[_,_],_) -> ?ex_both end,
                      set_elem=fun(Tctx,_,Val,_) ->
                                       Prots = case Val of
                                                   ?CONFD_ENUM_VALUE(?ex_ipv4) -> [ipv4];
                                                   ?CONFD_ENUM_VALUE(?ex_ipv6) -> [ipv6];
                                                   ?CONFD_ENUM_VALUE(?ex_both) -> [ipv4,ipv6]
                                               end,
                                       ec_genet:switch_discriminator(Tctx,
                                                                     [union,'split-by-rows',lists,structure,?LLROOT],
                                                                     [[{KB},ipv4], [{KB},ipv6]],
                                                                     [[{KB},Prot] || Prot <- Prots])
                               end
                     };
        [d,{KB},a,union,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      nested=[#mappings{path=[n,{KB},ipv4,union,'split-by-rows',lists,structure,?LLROOT]},
                              #mappings{path=[n,{KB},ipv6,union,'split-by-rows',lists,structure,?LLROOT]}],
                      fupval=fun(_,_,LLVals,_) -> ec_genet:join_pick_not_equals(not_found,LLVals) end
                     };

        %% Here we retrieve entries from two LL lists and use them all one-by-one; the
        %% missing key for the HL list entry is given by the LL source of the entry, hence
        %% we need to know where the key originated - using composite_list_key_fn
        [a,join,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{nested=[#mappings{get_next=fun ec_genet:composite_list_get_next/4,
                                        extra=[{composite_list_get_next_mode,each},
                                               {composite_list_get_next,
                                                [[ipv4,join,'split-by-rows',lists,structure,?LLROOT],
                                                 [ipv6,join,'split-by-rows',lists,structure,?LLROOT]]},
                                               {composite_list_key_fn,
                                                fun({Appl},[ipv4|_]) -> {Appl, ?CONFD_ENUM_VALUE(?ex_ipv4)};
                                                   ({Appl},[ipv6|_]) -> {Appl, ?CONFD_ENUM_VALUE(?ex_ipv6)} end}]
                                       }],
                      fupkeys=fun(_,_,[{false,undefined}],_) ->
                                      {false,undefined};
                                 (_,_,[{Key,C}],_) ->
                                      {Key,C}
                              end
                     };
        [{Appl,?CONFD_ENUM_VALUE(?ex_ipv4)},a,join,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{path=[{Appl},ipv4,join,'split-by-rows',lists,structure,?LLROOT]};
        [{Appl,?CONFD_ENUM_VALUE(?ex_ipv6)},a,join,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{path=[{Appl},ipv6,join,'split-by-rows',lists,structure,?LLROOT]};
        [b,{Appl,_},a,join,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                     relpath=[m],
                     fupval=fun(_,_,not_found,_) -> not_found;
                               (_,_,_,_) -> Appl end};
        [d,{_,_},a,join,'split-by-rows',lists,structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                     relpath=[n]};

        [a,sequencing,lists,structure,?HLROOT] ->
            #mappings{get_next=ec_genet:ordering_get_next([k,sequencing,lists,structure,?LLROOT],
                                                          fun ({KBin}) ->
                                                                  {?CONFD_UINT32(list_to_integer(binary_to_list(KBin)))}
                                                          end)};
        [{?CONFD_UINT32(X)},a,sequencing,lists,structure,?HLROOT] ->
            #mappings{path=[{list_to_binary(integer_to_list(X))},k,sequencing,lists,structure,?LLROOT]};
        [b,{_},a,sequencing,lists,structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[l],
                      fupval=fun(_,_,not_found,_) -> not_found;
                                (_,_,KBin,_) -> ?CONFD_UINT32(list_to_integer(binary_to_list(KBin))) end};
        [c,{_},a,sequencing,lists,structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[m]};

        %% Mapping a nested list to flat list requires handling of several cases, some of
        %% them a bit tricky; the macro NESTED_TO_FLAT tries to do that as generally as
        %% reasonable.
        ?NESTED_TO_FLAT([a,'nested-list-to-flat','nested-lists',structure,?HLROOT], % path to the list
                       b, c, d, % key name, sublist name, sublist key name
                       [k,'nested-list-to-flat','nested-lists',structure,?LLROOT], % LL flat list path
                       m, % LL list key name
                       1); % number of keys in the top level list
        [e,_K1,c,_K2,a,'nested-list-to-flat','nested-lists',structure,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[n]};

        %% Mapping a flat list to nested list is somewhat easier.  Only iterating over the
        %% list is a bit tricky, we are using nested_list_next for that; and we also need
        %% to deal with the outer list instances when we are creating a HL instance (in
        %% this case we need to make sure the appropriate outer list instance exists) and
        %% when deleting a HL instance (we decided to delete the outer list instance if it
        %% has no inner list instances).  For the latter two parts we are using
        %% composition of pre- and post- hook mappings generator.
        [a,'flat-list-to-nested','nested-lists',structure,?HLROOT] ->
            #mappings{get_next=fun(Tctx,_,Next,_) ->
                                      ec_genet:nested_list_next(Tctx, [k,'flat-list-to-nested','nested-lists',structure,?LLROOT], [m], Next)
                               end};
        [{B,C},a,'flat-list-to-nested','nested-lists',structure,?HLROOT] ->
            LLBase = [{B},k,'flat-list-to-nested','nested-lists',structure,?LLROOT],
            ec_genet_mapgens:pre_hook(
              ec_genet_mapgens:post_hook([{C},m|LLBase],
                                         #mappings{delete=fun(Tctx,_,_) ->
                                                                  ec_genet:delete_parent_if_empty(Tctx, LLBase, [m])
                                                          end}),
              #mappings{create=fun(Tctx,_,_) ->
                                       ec_genet:create_if_nonexist(Tctx, LLBase)
                               end});
        [b,{B,C},a,'flat-list-to-nested','nested-lists',structure,?HLROOT] ->
            #mappings{path=[n,{C},m,{B},k,'flat-list-to-nested','nested-lists',structure,?LLROOT],
                      fupval=ec_genet:value_fun(fun(_) -> B end)};
        [d,{B,C},a,'flat-list-to-nested','nested-lists',structure,?HLROOT] ->
            #mappings{path=[o,{C},m,{B},k,'flat-list-to-nested','nested-lists',structure,?LLROOT]};

        [a,'list-to-leaf-list',structure,?HLROOT] ->
            #mappings{get_next=fun ec_genet:constant_keyset_get_next/4,
                      extra=fun(Tctx) ->
                                    [{X} || X <- ec_genet:get_elem(Tctx, [k,'list-to-leaf-list',structure,?LLROOT], [])]
                            end};
        [{X},a,'list-to-leaf-list',structure,?HLROOT] ->
            LLPath = [k,'list-to-leaf-list',structure,?LLROOT],
            #mappings{create=fun(Tctx,_,_) ->
                                     Old = ec_genet:get_elem(Tctx, LLPath, []),
                                     case lists:member(X, Old) of
                                         true -> ok;
                                         _ -> ec_genet:set_elem(Tctx, LLPath, [X|Old])
                                     end
                             end,
                      delete=fun(Tctx,_,_) ->
                                     Old = ec_genet:get_elem(Tctx, LLPath, []),
                                     case lists:member(X, Old) of
                                         false -> ok;
                                         _ -> ec_genet:set_elem(Tctx, LLPath, lists:delete(X, Old))
                                     end
                             end};
        [b,{X},a,'list-to-leaf-list',structure,?HLROOT] ->
            #mappings{path=[k,'list-to-leaf-list',structure,?LLROOT],
                      fupval=ec_genet:value_fun(fun(List) ->
                                                        case lists:member(X, List) of
                                                            true -> X;
                                                            false -> not_found
                                                        end
                                                end)};

        %% For getting the value from LL, we need to look into both cases and pick the
        %% right result.  For setting the value, we send the value to the right case; but
        %% since something needs to be sent to both sub-mappings, we send a 'not_found'
        %% value to the other case and remap the operation to 'nop' there.
        [a,'address-value',nesting,?HLROOT] ->
            ec_genet_mapgens:compose(
              #mappings{fupval=ec_genet:value_fun(fun({true, Bin}) -> <<$0,Bin/binary>>;
                                                     ({false, Bin}) -> <<$1,Bin/binary>>
                                                  end),
                        fdnval=ec_genet:value_fun(fun(<<$0,Bin/binary>>) -> {true, Bin};
                                                     (<<$1,Bin/binary>>) -> {false, Bin}
                                                  end)},
              ec_genet_mapgens:address_switch([l,'address-value',nesting,?LLROOT],
                                              [m,'address-value',nesting,?LLROOT]));

        %% The choice handling is (almost) the same as for case map, the only difference
        %% is that we overload the cases.  Here we also use the fact that the latter takes
        %% precedence in case of a conflict, i.e. set_case(b) is mapped to
        %% set_elem(<<"b0">>).
        [[a],'existence-address',nesting,?HLROOT] ->
            ec_genet_mapgens:from_cases([k,'existence-address',nesting,?LLROOT],
                                        [{b,<<"b0">>}, {e,<<"e0">>}],
                                        [{<<"b0">>,b}, {<<"e0">>,e}, {<<"b1">>,b}, {<<"e1">>,e}]);

        %% Almost standard from_existence instance, except that the delete operation is
        %% mapped to delete (i.e. when the leaf c is deleted, the leaf k is deleted as
        %% well)
        [c,'existence-address',nesting,?HLROOT] ->
            ec_genet_mapgens:from_existence_delete(
              #mappings{path=[k,'existence-address',nesting,?LLROOT],
                        fupval=ec_genet:value_fun(fun(<<X/integer,_/binary>>) -> <<X/integer,$0>> end)},
              <<"b0">>);
        %% Standard from_existence, except that all get_elem and delete operations are
        %% guarded by that the target leaf value must be "b1" (otherwise the operations
        %% are ignored/not_found is returned).
        [d,'existence-address',nesting,?HLROOT] ->
            ec_genet_mapgens:guard_by_value(
              ec_genet_mapgens:from_existence([k,'existence-address',nesting,?LLROOT],
                                              <<"b0">>,
                                              <<"b1">>),
              [k,'existence-address',nesting,?LLROOT],
              <<"b1">>);
        [f,'existence-address',nesting,?HLROOT] ->
            ec_genet_mapgens:from_existence_delete(
              #mappings{path=[k,'existence-address',nesting,?LLROOT],
                        fupval=ec_genet:value_fun(fun(<<X/integer,_/binary>>) -> <<X/integer,$0>> end)},
              <<"e0">>);
        [g,'existence-address',nesting,?HLROOT] ->
            ec_genet_mapgens:guard_by_value(
              ec_genet_mapgens:from_existence([k,'existence-address',nesting,?LLROOT],
                                              <<"e0">>,
                                              <<"e1">>),
              [k,'existence-address',nesting,?LLROOT],
              <<"e1">>);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Existence Domain Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        [a,constant,existence,?HLROOT] ->
            ec_genet_mapgens:exists_constant(HLPath, false);
        [a,'sentinel-hl',sentinel,existence,?HLROOT] ->
            ec_genet_mapgens:sentinel_hl(HLPath, [k,'sentinel-hl',sentinel,existence,?LLROOT], 0);
        [a,'sentinel-ll',sentinel,existence,?HLROOT] ->
            LLPath = [k,'sentinel-ll',sentinel,existence,?LLROOT],
            ec_genet_mapgens:sentinel_ll(LLPath, LLPath, "");
        [a,'boolean-to-existence',boolean,existence,?HLROOT] ->
            ec_genet_mapgens:to_existence([k,'boolean-to-existence',boolean,existence,?LLROOT], false, true);
        [a,'existence-to-enum',boolean,existence,?HLROOT] ->
            LLPath = [k,'existence-to-enum',boolean,existence,?LLROOT],
            %% this time we want the values to be converted (atoms are not a representation of enums)
            ec_genet_mapgens:from_existence(LLPath, LLPath, notexists, exists);

        [[a],'case-map',existence,?HLROOT] ->
            ec_genet_mapgens:from_cases([k,'case-map',existence,?LLROOT],
                                        [{b,?CONFD_ENUM_VALUE(?exdev_case_b)},
                                         {e,?CONFD_ENUM_VALUE(?exdev_case_e)}]);
        [c,'case-map',existence,?HLROOT] ->
            ec_genet_mapgens:guard_by_value([l,'case-map',existence,?LLROOT],
                                            [k,'case-map',existence,?LLROOT],
                                            ?CONFD_ENUM_VALUE(?exdev_case_b));
        [d,'case-map',existence,?HLROOT] ->
            ec_genet_mapgens:guard_by_value([m,'case-map',existence,?LLROOT],
                                            [k,'case-map',existence,?LLROOT],
                                            ?CONFD_ENUM_VALUE(?exdev_case_b));
        [f,'case-map',existence,?HLROOT] ->
            %% here we need to delete the int32 LL leaf if this element is set - this is
            %% task of the nested mappings - the first one sets/retrieves the value (and
            %% only this value is used in fupval), the second deletes in case of the set
            %% operation
            ec_genet_mapgens:guard_by_value(#mappings{nested=[#mappings{path=[l,'case-map',existence,?LLROOT]},
                                                              #mappings{path=[m,'case-map',existence,?LLROOT],
                                                                        fopmap=fun(Tctx,set_elem,Path,Arg,Mappings) ->
                                                                                       {Tctx,delete,Path,Arg,Mappings};
                                                                                  (Tctx,_,Path,Arg,Mappings) ->
                                                                                       {Tctx,nop,Path,Arg,Mappings}
                                                                               end}],
                                                      fupval=fun(_,_,[Val,_],_) -> Val end},
                                            [k,'case-map',existence,?LLROOT],
                                            ?CONFD_ENUM_VALUE(?exdev_case_e));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Address Domain Mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        [a,constant,address,?HLROOT] ->
            #mappings{path=[k,constant,address,?LLROOT]};

        %% For a list to list mapping where key value needs to be adjusted, we just need
        %% to provide fupkeys for the list itself (to be used when get_next is processed)
        %% and to map the key value in the path
        [a,'key-offset',address,?HLROOT] ->
            #mappings{path=[l,'key-offset',address,?LLROOT],
                      fupkeys=fun(_,_,Key,_) ->
                                      case Key of
                                          {V} -> {V-1};
                                          X -> X
                                      end
                              end};
        [{V},a,'key-offset',address,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[{V+1}]};
        [b,{_},a,'key-offset',address,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[k]};

        [a,'key-constant',address,?HLROOT] ->
            #mappings{path=[k,'key-constant',address,?LLROOT],
                      fupkeys=fun(_,_,{B},_) -> {B,?CONFD_UINT32(179)};
                                 (_,_,KVal,_) -> KVal end};
        [{X,?CONFD_UINT32(179)},a,'key-constant',address,?HLROOT] ->
            #mappings{path=[{X},k,'key-constant',address,?LLROOT]};
        [{_,_},a,'key-constant',address,?HLROOT] ->
            %% FIXME: should throw error immediately, not yet supported by genet
            #mappings{fopmap=fun(_,_,_,_,_) -> throw({error, "Only the value 179 is supported"}) end};
        [b,{_,_},a,'key-constant',address,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[l]};

        %% The basic mapping is simple - just pick the LL key value based on the
        %% subcontainer name; the catch is in that we need to create the
        %% instance before we can set the value, if the instance does not
        %% exists.
        [c,X,'key-container',address,?HLROOT] ->
            Key = case X of
                      a -> ?CONFD_ENUM_VALUE(?exdev_c1);
                      b -> ?CONFD_ENUM_VALUE(?exdev_c2)
                  end,
            LLBase = [{Key},k,'key-container',address,?LLROOT],
            ec_genet_mapgens:pre_hook([m|LLBase],
                                      #mappings{set_elem=fun(Tctx,_,_,_) ->
                                                                 ec_genet:create_if_nonexist(Tctx, LLBase)
                                                         end});

        %% mapping a leaf and a boolean to a choice; for the leaf, we use the
        %% address_switch generator, but we must process the return value (which is
        %% returned in the form {true|false, Value}), and the input needs to be provided
        %% in the same form.
        [a,choice,dynamic,address,?HLROOT] ->
            Choice = [choice,dynamic,address,?LLROOT],
            ec_genet_mapgens:compose(
              #mappings{fupval=ec_genet:value_fun(fun({_,Val}) -> Val end),
                        fdnval=fun(Tctx,_,Val,_) -> {ec_genet:exists(Tctx, [k|Choice]), Val} end},
              ec_genet_mapgens:address_switch([k|Choice],
                                              [l|Choice]));
        %% the boolean is handled similarly - for get, we use the first part (true or
        %% false) of the return value; for set, we use the other leaf's value (with empty
        %% string as default) and pair it with the boolean value.
        [d,choice,dynamic,address,?HLROOT] ->
            Choice = [choice,dynamic,address,?LLROOT],
            ec_genet_mapgens:compose(
              #mappings{fupval=ec_genet:value_fun(fun({X,_}) -> X end),
                        fdnval=fun(Tctx,_,Bool,_) ->
                                       OtherLeaf = if Bool -> l; true -> k end,
                                       {Bool, ec_genet:get_elem(Tctx, [OtherLeaf|Choice], <<>>)}
                               end},
              ec_genet_mapgens:address_switch([k|Choice],
                                              [l|Choice]));

        %% mapping one list to multiple lists based on the key value
        [l,lists,dynamic,address,?HLROOT] ->
            #mappings{get_next=ec_genet:joined_lists_next([[l1],[l2],[l3]],
                                                          [lists,dynamic,address,?LLROOT],
                                                          fun(_Tctx, [ListName], {Val}) ->
                                                                  %% ListName is e.g. l2 (an atom);
                                                                  %% Val is an integer, e.g. 33; we need to map
                                                                  %% that to "2-33"
                                                                  Str = string:join([tl(atom_to_list(ListName)),
                                                                                     integer_to_list(Val)],
                                                                                    "-"),
                                                                  {list_to_binary(Str)}
                                                          end)};
        [{V},l,lists,dynamic,address,?HLROOT] ->
            #mappings{path=[{V},l1,lists,dynamic,address,?LLROOT],
                      fdnpath=fun(_, [_,l1|List], _) ->
                                      [ListId, StrVal] = string:tokens(binary_to_list(V), "-"),
                                      ListName = list_to_atom([$l|ListId]),
                                      [{list_to_integer(StrVal)},ListName|List]
                              end};
        [X, {_},l,lists,dynamic,address,?HLROOT] ->
            #mappings{inherit=tl(HLPath),
                      relpath=[X]};

        %% The pattern below matches all other paths in the 
        %% trafo-examples.yang model. We return none for mapping
        %% record, which makes genet return not_found for any request.
        _ ->
            none
    end.

binary2hexstring(Bin) ->
    Bytes = lists:map(fun(Byte) -> integer_to_list(Byte, 16) end, binary_to_list(Bin)),
    list_to_binary(string:join(Bytes, ":")).

hexstring2binary(BString) ->
    Bytes = string:tokens(binary_to_list(BString), ":"),
    list_to_binary(lists:map(fun(StrByte) -> list_to_integer(StrByte, 16) end, Bytes)).

%%%===================================================================
%%% End
%%%===================================================================
