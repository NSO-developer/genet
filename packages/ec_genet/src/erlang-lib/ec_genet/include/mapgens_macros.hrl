-define(NESTED_TO_FLAT(TopListPath, TopListKey, NestedList, NestedListKey, LLListPath, LLListKey, KeyCount),
            TopListPath ->
                #mappings{get_next=ec_genet:get_next_flat_key(LLListPath, KeyCount)};
            [Keys|TopListPath] ->
                %% no action can be done for creating the upper of the two nested lists
                #mappings{create=fun(_,_,_) -> ok end,
                          delete=fun(Tctx,_,_) -> ec_genet:delete_flat_instances(Tctx, LLListPath, Keys) end};
            [TopListKey,KeyVal|TopListPath] ->
                #mappings{path=LLListPath,
                          fopmap=fun(Tctx,get_elem,Path,_,Mappings) ->
                                         {Tctx,find_next,Path,
                                          {ec_genet:init_cursor(Tctx, LLListPath), ?CONFD_FIND_SAME_OR_NEXT, KeyVal},
                                          Mappings}
                                 end,
                          fupval=fun(_,_,{LLKeys,_},_) -> case is_tuple(LLKeys) andalso ec_genet:tuple_prefix(KeyVal, LLKeys) of
                                                              true -> element(1, KeyVal);
                                                              false -> not_found
                                                          end
                                 end};
            [NestedList,KeyVal|TopListPath] ->
                #mappings{path=LLListPath,
                          fopmap=fun(Tctx,get_next,Path,-1,Mappings) ->
                                         {Tctx, find_next, Path,
                                          {ec_genet:init_cursor(Tctx, Path), ?CONFD_FIND_SAME_OR_NEXT, KeyVal},
                                          Mappings};
                                    (Tctx,get_next,Path,{Cursor,_,Last},Mappings) ->
                                         {Tctx, find_next, Path, {Cursor, ?CONFD_FIND_NEXT, Last}, Mappings}
                                 end,
                          fupkeys=fun(_,_,LLKeys,_) -> case is_tuple(LLKeys) andalso ec_genet:tuple_prefix(KeyVal, LLKeys) of
                                                           true -> ec_genet:subtuple(LLKeys, KeyCount+1, tuple_size(LLKeys) - KeyCount + 1);
                                                           false -> false
                                                       end
                                  end};
            [KeyVal1,NestedList,KeyVal2|TopListPath] ->
                #mappings{path=[ec_genet:append_tuples(KeyVal2,KeyVal1)|LLListPath]};
            [NestedListKey,_K1,NestedList,_K2|TopListPath] ->
                #mappings{inherit=tl(HLPath),
                          relpath=[LLListKey]}).
