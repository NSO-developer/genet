-type err() :: {'error', Reason :: any() }.

-record(mappings, {
          path = none :: econfd:ikeypath(),
          relpath = none :: econfd:ikeypath(),
          extra = none :: any(),
          inherit = none :: econfd:ikeypath(),
          nested = none :: [#mappings{}],
          fexists = none :: fun((Tctx :: #confd_trans_ctx{},
                                 HLPath :: econfd:ikeypath(),
                                 Extra :: any())
                                -> not_found | any()),
          fdnpath = none :: fun((Tctx :: #confd_trans_ctx{},
                                 KeyedLLPath :: econfd:ikeypath(),
                                 Extra :: any())
                                -> econfd:ikeypath()),
          fopmap = none :: fun((Tctx :: #confd_trans_ctx{},
                                Op :: atom(),
                                Path :: econfd:ikeypath(),
                                Arg :: any(),
                                Mappings :: #mappings{})
                               -> {#confd_trans_ctx{}, atom(), econfd:ikeypath(), any(), #mappings{}}),
          fupkeys = none :: fun((Tctx :: #confd_trans_ctx{},
                                 Op :: atom(),
                                 Keys :: econfd:key(),
                                 Extra :: any())
                                -> econfd:key()),
          fdnkeys = none :: fun((Tctx :: #confd_trans_ctx{},
                                 KeySet :: [econfd:key()],
                                 Extra :: any())
                                -> [econfd:key()]),
          fupval = none :: fun((Tctx :: #confd_trans_ctx{},
                                Op :: atom(),
                                RawVal :: econfd:value(),
                                Extra :: any())
                               -> econfd:value()),
          fdnval = none :: fun((Tctx :: #confd_trans_ctx{},
                                Op :: atom(),
                                RawVal :: econfd:value(),
                                Extra :: any())
                               -> econfd:value()),
          get_elem = none :: fun((Tctx :: #confd_trans_ctx{},
                                  HLPath :: econfd:ikeypath(),
                                  Extra :: any())
                                 -> econfd:value() | not_found | err()),
          exists = none :: fun((Tctx :: #confd_trans_ctx{},
                                HLPath :: econfd:ikeypath(),
                                Extra :: any())
                               -> boolean() | err()),
          create = none :: fun((Tctx :: #confd_trans_ctx{},
                                HLPath :: econfd:ikeypath(),
                                Extra :: any())
                               -> ok | err()),
          delete = none :: fun((Tctx :: #confd_trans_ctx{},
                                HLPath :: econfd:ikeypath(),
                                Extra :: any())
                               -> ok | err()),
          move_after = none :: fun((Tctx :: #confd_trans_ctx{},
                                    HLPath :: econfd:ikeypath(),
                                    PrevKeys :: {econfd:value()},
                                    Extra :: any())
                                   -> ok | err()),
          set_elem = none :: fun((Tctx :: #confd_trans_ctx{},
                                  HLPath :: econfd:ikeypath(),
                                  Val :: econfd:value(),
                                  Extra :: any())
                                 -> ok | err()),
          get_next = none :: fun((Tctx :: #confd_trans_ctx{},
                                  HLPath :: econfd:ikeypath(),
                                  C :: econfd:maapi_cursor(),
                                  Extra :: any())
                                 -> {false,undefined} |
                                    {ok, econfd:key(), econfd:maapi_cursor()} |
                                    err()),
          get_case = none :: fun((Tctx :: #confd_trans_ctx{},
                                  HLPath :: econfd:ikeypath(),
                                  Choice :: econfd:qtag() | [econfd:qtag()],
                                  Extra :: any())
                                 -> econfd:qtag() | err()),
          set_case = none :: fun((Tctx :: #confd_trans_ctx{},
                                  HLPath :: econfd:ikeypath(),
                                  Choice :: econfd:qtag() | [econfd:qtag()],
                                  econfd:qtag(),
                                  Extra :: any())
                                 -> ok | err())
         }).
