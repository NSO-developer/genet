-define(GENET_TRACE_LEVEL, ?CONFD_DEBUG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LOG(Level,Msg),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s", [Msg]])).
-define(LOG(Level,Msg, Arg1),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s~n ~s=~p",
                                     [Msg, ??Arg1, Arg1]])).
-define(LOG(Level,Msg, Arg1, Arg2),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s~n ~s=~p~n ~s=~p",
                                     [Msg, ??Arg1, Arg1, ??Arg2, Arg2]])).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s~n ~s=~p~n ~s=~p~n ~s=~p",
                                     [Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3]])).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3, Arg4),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p",
                                     [Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3, ??Arg4, Arg4]])).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3, Arg4, Arg5),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p",
                                     [Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3, ??Arg4, Arg4, ??Arg5, Arg5]])).
-define(LOG(Level,Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6),
        ec_genet_advices:format_log(Level, ?MODULE, ?LINE, fun io_lib:format/2,
                                    ["LOG ~s~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p~n ~s=~p",
                                     [Msg, ??Arg1, Arg1, ??Arg2, Arg2, ??Arg3, Arg3, ??Arg4, Arg4, ??Arg5, Arg5, ??Arg6, Arg6]])).

-define(LOGERR(Msg), ?LOG(error, Msg)).
-define(LOGERR(Msg, Arg1), ?LOG(error, Msg, Arg1)).
-define(LOGERR(Msg, Arg1, Arg2), ?LOG(error, Msg, Arg1, Arg2)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3), ?LOG(error, Msg, Arg1, Arg2, Arg3)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(error, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(error, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGERR(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(error, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGWARN(Msg), ?LOG(warn, Msg)).
-define(LOGWARN(Msg, Arg1), ?LOG(warn, Msg, Arg1)).
-define(LOGWARN(Msg, Arg1, Arg2), ?LOG(warn, Msg, Arg1, Arg2)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3), ?LOG(warn, Msg, Arg1, Arg2, Arg3)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(warn, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(warn, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGWARN(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(warn, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGNOTE(Msg), ?LOG(note, Msg)).
-define(LOGNOTE(Msg, Arg1), ?LOG(note, Msg, Arg1)).
-define(LOGNOTE(Msg, Arg1, Arg2), ?LOG(note, Msg, Arg1, Arg2)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3), ?LOG(note, Msg, Arg1, Arg2, Arg3)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(note, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(note, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGNOTE(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(note, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGINFO(Msg), ?LOG(info, Msg)).
-define(LOGINFO(Msg, Arg1), ?LOG(info, Msg, Arg1)).
-define(LOGINFO(Msg, Arg1, Arg2), ?LOG(info, Msg, Arg1, Arg2)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3), ?LOG(info, Msg, Arg1, Arg2, Arg3)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(info, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(info, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGINFO(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(info, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

-define(LOGMSG(Msg), ?LOG(debug, Msg)).
-define(LOGMSG(Msg, Arg1), ?LOG(debug, Msg, Arg1)).
-define(LOGMSG(Msg, Arg1, Arg2), ?LOG(debug, Msg, Arg1, Arg2)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3), ?LOG(debug, Msg, Arg1, Arg2, Arg3)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4), ?LOG(debug, Msg, Arg1, Arg2, Arg3, Arg4)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4, Arg5), ?LOG(debug, Msg, Arg1, Arg2, Arg3, Arg4, Arg5)).
-define(LOGMSG(Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), ?LOG(debug, Msg, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ASSERT(Expected,Actual),
	case Actual of
		Expected -> ok;
		_ -> io:format("~nASSERTION FAILED ~s:~p~nExpected=~p~n  Actual=~p~n", 
                  [?MODULE, ?LINE, Expected, Actual]), Expected=Actual
	end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
