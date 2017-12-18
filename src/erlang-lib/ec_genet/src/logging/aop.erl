%% -*- erlang-indent-level: 8 -*-

%%
%% Copyright (C) 2011 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%% @since 2011-06-13
%% @copyright 2011 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc
%% This is a main module of AOP for Erlang tool.
%% The module contains exported function compile/2 and compile/3.
%%
%% @reference [http://static.springsource.org/spring/docs/2.0.8/reference/aop.html]
%% @headerfile "aop.hrl"
-module(aop).

%%
%% Include files
%%
-include("aop.hrl").

%%
%% Import modules
%%
-import(compile, []).
-import(filelib, []).
-import(file, []).
-import(lists, []).

%%
%% Exported Functions
%%
-export([parse_transform/2]).

%%
%% API Functions
%%

parse_transform(Forms, Options) ->
        case read_groups_config(proplists:get_value(aop_groups_module, Options),
                                proplists:get_value(aop_config_dir, Options)) of
                none ->
                        Forms;
                Aspects ->
                        weaver:parse_transform(Forms, [{aop_config, Aspects}])
        end.

read_groups_config(Module, Dir) when Module /= undefined, is_list(Dir) ->
        Groups = apply(Module, log_groups, []),
        ADFFiles = filelib:fold_files(Dir, ".*\.adf$", true, fun(F, Acc) -> [F | Acc] end, []),
        read_adf(Groups, ADFFiles);
read_groups_config(_, _) ->
        none.

%% @spec read_adf(Groups::list(), Files::list()) -> list(#aspect{})
%% @doc reads adf file and converts its content to Erlang term.
%%
read_adf(Groups, Files) ->
        Gr = {'Groups', Groups},
        As = {'Aspect', fun(Ad, Pcs) -> #aspect{advice=Ad, pointcuts=Pcs} end},
        Pc = {'Pointcut', fun(M,F,A,S) -> #pointcut{module=M, function=F, arity=A, type=S} end},
        Ad = {'Advice', fun(T, M, F, A) -> #advice{type=T, module=M, function=F, arguments=A} end},
        Bindings = [Gr,Ad,As,Pc],
        read_adf(Files, Bindings, [])
.

%% @spec read_adf(Files::list(), Bindings::list(), Result::list()) -> list(#aspect{})
%% @doc reads adf file and converts its content to Erlang term.
%%
read_adf([], _, Result) -> Result;
read_adf([File | Files], Bindings, Result) ->
        case file:script(File, Bindings) of
                {ok, S} ->
                        read_adf(Files, Bindings, lists:append(S, Result));
                {error, Error} ->
                        io:format(standard_error, "Read adf error = ~p~n~p~n~p~n", [Error, File, Bindings]),
                        throw({error, "adf file error"})
        end
.
