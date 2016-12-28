-define(GENET_TRACE_LEVEL, ?CONFD_DEBUG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LOGMSG(Msg), void).
-define(LOGMSG(Msg, Arg1), void).
-define(LOGMSG(Msg, Arg1, Arg2), void).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3), void).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4), void).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), void).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), void).

-define(LOG(Arg1), void).
-define(LOG(Arg1, Arg2), void).
-define(LOG(Arg1, Arg2, Arg3), void).
-define(LOG(Arg1, Arg2, Arg3, Arg4), void).
-define(LOG(Arg1, Arg2, Arg3, Arg4, Arg5), void).
-define(LOG(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), void).
-define(LOG(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7), void).

-define(LOGERR(Msg), void).
-define(LOGERR(Msg, Arg1), void).
-define(LOGERR(Msg, Arg1, Arg2), void).
-define(LOGERR(Msg, Arg1, Arg2, Arg3), void).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4), void).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), void).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), void).

-define(LOGWARN(Msg), void).
-define(LOGWARN(Msg, Arg1), void).
-define(LOGWARN(Msg, Arg1, Arg2), void).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3), void).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4), void).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), void).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), void).

-define(LOGNOTE(Msg),  void).
-define(LOGNOTE(Msg, Arg1), void).
-define(LOGNOTE(Msg, Arg1, Arg2), void).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3), void).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4), void).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), void).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), void).

-define(LOGINFO(Msg), void).
-define(LOGINFO(Msg, Arg1), void).
-define(LOGINFO(Msg, Arg1, Arg2), void).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3), void).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4), void).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), void).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), void).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ASSERT(Expected,Actual),
	case Actual of
		Expected -> ok;
		_ -> io:format("~nASSERTION FAILED ~s:~p~nExpected=~p~n  Actual=~p~n", 
                  [?MODULE, ?LINE, Expected, Actual]), Expected=Actual
	end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
