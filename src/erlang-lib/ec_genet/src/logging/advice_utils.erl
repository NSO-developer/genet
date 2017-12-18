-module(advice_utils).
-export([log_aspects/4]).
-include("log_groups.hrl").

log_aspects({Groups,Levels}, Aspect,Advice,Pointcut) ->
    lists:flatten([group_aspects(Group, proplists:get_value(Group, Levels), Modules, {Aspect, Advice, Pointcut}) ||
                      {Group, Modules} <- Groups]).

group_aspects(Group, Level, Modules, Callbacks) ->
    [module_aspects(Group, Level, Module, Functions, Callbacks) || {Module, Functions} <- Modules].

module_aspects(Group, Level, Module, Functions, {Aspect,Advice,Pointcut}) ->
    Funs = fun_list(Functions),
    Modname = "^" ++ atom_to_list(Module) ++ "$",
    [Aspect(Advice(before, ec_genet_logger, before_advice, [Group, Level]),
            [Pointcut(Modname, Funs, "*", global)]),
     Aspect(Advice(after_return, ec_genet_logger, after_advice, [Group, Level]),
            [Pointcut(Modname, Funs, "*", global)]),
     Aspect(Advice(after_throw, ec_genet_logger, throw_advice, [Group]),
            [Pointcut(Modname, Funs, "*", global)])].

fun_list(Names) ->
    lists:flatten(
      io_lib:format("^(~s)$", [string:join(lists:map(fun erlang:atom_to_list/1, Names), "|")])).
