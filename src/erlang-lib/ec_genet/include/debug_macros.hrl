-define(GENET_TRACE_LEVEL, ?CONFD_DEBUG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LOG(Level,Msg),
        econfd:log(Level, "LOG ~p:~p ~s", [?MODULE, ?LINE,
                                           Msg]
                   )).
-define(LOG(Level,Msg, Arg1),
        econfd:log(Level, "LOG ~p:~p ~s~n ~s=~p", [?MODULE, ?LINE,
                                                   Msg, ??Arg1, Arg1]
                   )).
-define(LOG(Level,Msg, Arg1, Arg2),
        econfd:log(Level, "LOG ~p:~p ~s~n ~s=~p~n ~s=~p", [?MODULE, ?LINE,
                                                           Msg, ??Arg1, Arg1, ??Arg2, Arg2]
                   )).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3),
        econfd:log(Level, "LOG ~p:~p ~s~n ~s=~p~n ~s=~p~n ~s=~p", [?MODULE, ?LINE,
                                                                   Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3]
                   )).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3, Arg4),
        econfd:log(Level, "LOG ~p:~p ~s~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p", [?MODULE, ?LINE,
                                                                           Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3, ??Arg4, Arg4]
                   )).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3, Arg4, Arg5),
        econfd:log(Level, "LOG ~p:~p ~s~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p", [?MODULE, ?LINE,
                                                                                   Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3, ??Arg4, Arg4, ??Arg5, Arg5]
                   )).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6),
        econfd:log(Level, "LOG ~p:~p ~s~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p", [?MODULE, ?LINE,
                                                                                           Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3, ??Arg4, Arg4, ??Arg5, Arg5, ??Arg6, Arg6]
                   )).

-define(LOGERR(Msg), ?LOG(?CONFD_LEVEL_ERROR, Msg)).
-define(LOGERR(Msg, Arg1), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1)).
-define(LOGERR(Msg, Arg1, Arg2), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGWARN(Msg), ?LOG(?CONFD_LEVEL_ERROR, Msg)).
-define(LOGWARN(Msg, Arg1), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1)).
-define(LOGWARN(Msg, Arg1, Arg2), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(?CONFD_LEVEL_ERROR, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGNOTE(Msg), ?LOG(?CONFD_LEVEL_INFO, Msg)).
-define(LOGNOTE(Msg, Arg1), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1)).
-define(LOGNOTE(Msg, Arg1, Arg2), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGINFO(Msg), ?LOG(?CONFD_LEVEL_INFO, Msg)).
-define(LOGINFO(Msg, Arg1), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1)).
-define(LOGINFO(Msg, Arg1, Arg2), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(?CONFD_LEVEL_INFO, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGMSG(Msg), ?LOG(?CONFD_LEVEL_TRACE, Msg)).
-define(LOGMSG(Msg, Arg1), ?LOG(?CONFD_LEVEL_TRACE, Msg, Arg1)).
-define(LOGMSG(Msg, Arg1, Arg2), ?LOG(?CONFD_LEVEL_TRACE, Msg, Arg1, Arg2)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3), ?LOG(?CONFD_LEVEL_TRACE, Msg, Arg1, Arg2, Arg3)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(?CONFD_LEVEL_TRACE, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(?CONFD_LEVEL_TRACE, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(?CONFD_LEVEL_TRACE, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ASSERT(Expected,Actual),
	case Actual of
		Expected -> ok;
		_ -> io:format("~nASSERTION FAILED ~s:~p~nExpected=~p~n  Actual=~p~n", 
                  [?MODULE, ?LINE, Expected, Actual]), Expected=Actual
	end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
