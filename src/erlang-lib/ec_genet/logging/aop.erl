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
-export([compile/2, compile/3, compile_one_file/3, parse_transform/2]).

%%
%% API Functions
%%

%% @spec compile(Src_dirs::list(), Config_dirs::list()) -> ok
%% @doc
%%      Src_dirs = list(string()) - list of directory names these contain source files,
%%      Config_dirs = list(string()) - list of directory names these contain configuration files.
%% The function compiles a set of source files (*.erl) from source directories and applyes
%% AOP weaving during compilation stage. Configuration files for AOP weaving are taken
%% from configuration directories (*.adf)
%%
compile(Src_dirs, Config_dirs) ->
        compile(Src_dirs, Config_dirs, [])
.

%% @spec compile(Src_dirs::list(), Config_dirs::list(), Options::list()) -> ok
%% @doc
%%      Src_dirs = list(string()) - list of directory names these contain source files,
%%      Config_dirs = list(string()) - list of directory names these contain configuration files,
%%      Options = list() - Options determine the behavior of the compiler as defined in compile:file/2.
%% The function compiles a set of source files (*.erl) from source directories and applyes
%% AOP weaving during compilation stage. Configuration files for AOP weaving are taken
%% from configuration directories (*.adf)
%%
compile([], _, _) -> ok;
compile(Src_dirs, Config_dirs, Options) ->
        File_list = get_source_files(Src_dirs, []),
        io:format("Sources = ~p ~n", [File_list]),
        Opts = get_config_options(Config_dirs, Options),
        compile_files(File_list, Opts)
.

get_config_options(Config_dirs, Options) ->
        Config = get_configurations(Config_dirs, []),
        lists:append([{parse_transform, weaver}, return, report, {aop_config, Config}],
                     Options).

%% @spec compile_files(FileList::list(), Options::list()) -> ok
%% @doc compile list of files with Options.
%%
compile_files([], _) -> ok;
compile_files([File | FileList], Options) ->
        compile_one_file(File, Options),
        compile_files(FileList, Options)
.

parse_transform(Forms, Options) ->
        ConfigDirs =
                case proplists:get_value(aop_config_dir, Options) of
                        undefined -> [];
                        true -> [];
                        Dir -> [Dir]
                end,
        weaver:parse_transform(Forms, get_config_options(ConfigDirs, Options)).

compile_one_file(File, Config_dirs, Options) ->
        compile:file(File, get_config_options(Config_dirs, Options)).

compile_one_file(File, Options) ->
        case compile:file(File, Options) of
                {ok, ModuleName} -> io:format("Module name = ~p is weaved.~n", [ModuleName]);
                {ok, ModuleName, Warnings} -> io:format("Module name = ~p is weaved.\t Warnings = ~p~n",
                                                        [ModuleName, Warnings]);
                error -> io:format("error~n", []);
                {error, Errors, Warnings} -> io:format("Errors = ~p Warnings = ~p~n", [Errors, Warnings])
        end
.

get_source_files([], Result) -> Result;
get_source_files([Dir | Dirs], Result) ->
%       io:format(">>> get_source_files ~p~n", [Dir]),
        File_names = filelib:fold_files(Dir, ".*\.erl$", true, fun(F, Acc) -> [F | Acc] end, []),
        get_source_files(Dirs, lists:append(File_names, Result))
.

%% @spec get_configurations(Dirs::list(), Result::list()) -> list(#aspect{})
%% @doc Gets configuration files from list of directories and merges it to one
%% config list.
get_configurations([], Result) -> Result;
get_configurations([Dir | Dirs], Result) ->
        ADF_files = filelib:fold_files(Dir, ".*\.adf$", true, fun(F, Acc) -> [F | Acc] end, []),
        % io:format("Config files  = ~p~n", [ADF_files]),
        get_configurations(Dirs, lists:append(read_adf(ADF_files), Result))
.

%% @spec read_adf(Files::list()) -> list(#aspect{})
%% @doc reads adf file and converts its content to Erlang term.
%%
read_adf(Files) ->
        As = {'Aspect', fun(Ad, Pcs) -> {aspect, Ad, Pcs} end},
        Pc = {'Pointcut', fun(M,F,A,S) -> {pointcut, M, F, A, S} end},
        Ad = {'Advice', fun(T, M, F) -> {advice, T, M, F} end},
        Bindings = [Ad,As,Pc],
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
