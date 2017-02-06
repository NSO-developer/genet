-define(DPAPIs,
        [get_elem, get_case, set_elem, set_case, 'case', create,
         move_after, delete, exists, get_next]).

-define(ServerProcessing,
        [process_mapping,process_mapping_fexists,process_mapping_override,
         process_mapping_core,process_nested,path_rewrite,process_path,
         process_opmap,process_value,default_ll_op]).

-define(ServerAux,
        [get_mappings,get_mapping_record,get_module_prefix,get_mapping_fun,reg_mapping]).

-define(ServerOther,
        [format_error,extract_keys,inject_keys,inject_keys_int,
         tctx_maapi_sock,tctx_maapi_thandle,s_init,s_finish]).

-define(ServerConvert,
        [convert_value,do_convert_value,convert_other,convert_int_value,convert_buf_value,
         convert_enum_value]).

-define(GenetAux,
        [exists,get_elem_str,get_elem,get_case,set_elem_str,set_elem,create,
         create_if_nonexist,delete,delete_if_exists,move,init_cursor,get_next,
         composite_list_get_next,composite_list_advance_state_union,
         composite_list_advance_state,composite_list_initial_state,switch_discriminator,
         join_prio,join_pick,join_pick_not_equals,joined_lists_next,get_dynamic_list_next]).

-define(GenetOther,
        [val2str,str2val,int2decimal64,decimal642int,dup,get_first_keyset,
         value_fun,int_value_fun,run_initiate_dynamic_next]).

-define(MapgensAux, [process_composed_keys,extract_composed_keys,compose_list]).

-define(ACLAux, [get_permit_deny,name2seq]).

%% Log group identifies a set of functions in a module.  Any function that is supposed to
%% be logged needs to belong to a log group; any function in a group is compiled with a
%% code to perform logging when the particular log level is turned on.
-define(LOG_GROUPS,
        [{dpapi, [{ec_genet_server, ?DPAPIs}]},
         {processing, [{ec_genet_server, ?ServerProcessing}]},
         {calls,      [{ec_genet, ?GenetAux}]},
         {auxiliary, [{ec_genet_server, ?ServerAux},
                      {ec_genet_server, ?ServerConvert},
                      {ec_map_syslog_helper, [severity]},
                      {ec_map_acl, ?ACLAux},
                      {ec_genet_mapgens, ?MapgensAux}]}]).

%% Mapping from log group name to log level.  A function, so as to be logged, needs to
%% belong to a group, and that group needs to be assigned to a log level that is turned on
%% in runtime.
-define(LOG_LEVELS, [{dpapi, note},
                     {calls, info},
                     {processing, debug},
                     {auxiliary, debug}]).
