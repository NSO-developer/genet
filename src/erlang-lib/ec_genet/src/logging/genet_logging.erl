%%% @doc Logging group definitions, supposed to be used in compile
%%% time by aop.
%%%
%%% This module needs to export function `log_groups/0' which returns
%%% a tuple consisting of log groups and assignment of groups to log
%%% levels.  The value is used in compile time to instrument functions
%%% with "advices".
%%% @end

-module(genet_logging).

-export([log_groups/0]).
-include("log_groups.hrl").

log_groups() ->
    {?LOG_GROUPS, ?LOG_LEVELS}.
