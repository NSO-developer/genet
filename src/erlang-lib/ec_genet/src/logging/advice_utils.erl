-module(advice_utils).
-export([log_aspects/3]).
-include("log_groups.hrl").

log_aspects(Aspect,Advice,Pointcut) ->
    ModFuns = [element(2, lists:keyfind(Group, 1, ?LOG_GROUPS)) || {Group, _Level} <- ?LOG_LEVELS],
    lists:flatten(lists:map(fun (Mod) -> module_aspects({Aspect,Advice,Pointcut},Mod) end, ModFuns)).

module_aspects({Aspect,Advice,Pointcut}, {Module, Functions}) ->
    Funs = fun_list(Functions),
    Modname = "^" ++ atom_to_list(Module) ++ "$",
    [Aspect(Advice(before, ec_genet_logger, before_advice),
            [Pointcut(Modname, Funs, "*", global)]),
     Aspect(Advice(after_return, ec_genet_logger, after_advice),
            [Pointcut(Modname, Funs, "*", global)]),
     Aspect(Advice(after_throw, ec_genet_logger, throw_advice),
            [Pointcut(Modname, Funs, "*", global)])];
module_aspects(AAP, ModList) when is_list(ModList) ->
    lists:map(fun (Mod) -> module_aspects(AAP, Mod) end, ModList).

fun_list(Names) ->
    lists:flatten(
      io_lib:format("^(~s)$", [string:join(lists:map(fun erlang:atom_to_list/1, Names), "|")])).
