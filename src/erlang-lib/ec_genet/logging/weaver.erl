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

%% @since 2011-06-20
%% @copyright 2011 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc 
%% This module provides parse_transform/2 function. Compiler using the function as a plug-in
%% to weaving source code and to embedd AOP code in matched functions.
%%
-module(weaver).

%%
%% Include files
%%
-include("aop.hrl").

%%
%%
%% Import modules
%%
-import(proplists, []).
-import(re, []).
-import(lists, []).
-import(string, []).

-import(fun_proxy, []).

%% Exported Functions
%%
-export([parse_transform/2]).

%%
%% API Functions
%%

%% @spec parse_transform(Forms::list(), Options::list()) -> list(Form)
%% @throws nothing
%% @doc Erlang compiler invokes the function and passes AF tree formes along with Options.
%% The function changes AF tree formes according with AOP configuration and return them to compiler.
parse_transform(Forms, Options) -> 
	Defs = proplists:get_value(aop_config, Options, []),
%	io:format("Defs = ~p~n", [Defs]),
	try
%	io:format("Forms = ~p~n", [Forms]),
		Func_to_process = analyze_forms(Forms, Defs),
%	io:format("Func_to_process = ~p~n", [Func_to_process]),
		process_forms(Forms, Func_to_process, add_to_export(Func_to_process))
	catch
		E:R -> io:format("Exception = ~p, ~p~n", [E,R]),
		Forms
	end
.

%%
%% Local Functions
%%

%% @spec analyze_forms(Forms::list(), Aspects::list()) -> list(tuple())
%% @throws nothing
%% @doc Retrives from form list descriptions of all functions these match 
%% AOP configurations. Return list of tuples in form {Func_name, Arity, Aspects::list(#aspect{})}.
%%
analyze_forms(Forms, Aspects) ->
	analyze_forms(Forms, Aspects, void, [])
.

%% @spec analyze_forms(Forms::list(), Aspects::list(), Module::atom(), Result::list()) ->  list(tuple())
%% @throws nothing
%% @equiv weaver:analyze_forms/2 
%%
analyze_forms([], _Aspects, _, Result) -> Result;
analyze_forms([{attribute, _, module, Mod} | Forms], Aspects, _Module, Result) ->
	analyze_forms(Forms, Aspects, Mod, Result);
analyze_forms([{attribute, _, export, Functions} | Forms], Aspects, Module, Result) ->
	Func_list = [{Func_name, Arity, select(Module, F, Aspects)} || {Func_name, Arity} = F <- Functions],
	Func_list_1 = [X || {_,_,L} = X <- Func_list, not (length(L) == 0)],
	analyze_forms(Forms, Aspects, Module, lists:append(Func_list_1, Result));
analyze_forms([{function, _L, Name, Arity, _Fun_clauses} | Forms], Aspects, Module, Result) ->
	New_result =
	case length([true || {N, A, _} <- Result, (N =:= Name) and (A =:= Arity)]) of
		0 ->
			Aspects_list = select(Module, {Name, Arity}, Aspects),
			if length(Aspects_list) > 0 ->
					[{Name, Arity, Aspects_list} | Result];
				true -> Result
			end;
		_ -> Result
	end,
	analyze_forms(Forms, Aspects, Module, New_result);
analyze_forms([_ | Forms], Aspects, Module, Result) ->
	analyze_forms(Forms, Aspects, Module, Result)
.

%% @spec select(Module::atom(), Func::tuple(), Aspects::list(#aspect{})) -> Aspects::list(#aspect{}) 
%% Func = {Name::atom(), Arity::integer()}
%%
%% @throws nothing
%% @doc Extracts from Aspects list all aspects that matches the function Module:Func
%%
select(Module, {Func_name, Arity}, Aspects) ->
	F =
		fun(#aspect{pointcuts = Pointcuts_list}) -> 
			lists:any(
				fun(Pc) -> is_pointcut_match(Module, {Func_name, Arity}, Pc) end, 
				Pointcuts_list
			)
		end,
	[Aspect || Aspect <- Aspects, F(Aspect)]
.

%% @spec is_pointcut_match(Module::atom(), Func::tuple(), Pointcut::#pointcut{}) -> boolean() 
%% Func = {Func_name::atom(), Arity::integer()}
%% @throws nothing
%% @doc checks if function Module:Func_name/Arity matches Pointcut.
%%
is_pointcut_match(Module, {Func_name, Arity}, #pointcut{module=M_name, function=F_name, arity=Ar}) ->
	{ok, MP_module} = re:compile(M_name),
	{ok, MP_func} = re:compile(F_name),
	Options = [notempty,{capture,none}],
	((re:run(atom_to_list(Module), MP_module, Options) =:= match)
		and (re:run(atom_to_list(Func_name), MP_func, Options) =:= match)
		and (is_arity_match(Arity, Ar)))
.

%% @spec is_arity_match(Arity::integer(), Arity_regexp) -> boolean()
%% Arity_regexp = integer() | string()
%% @throws nothing
%% @doc checks if Arity matches Arity_regexp.
%%
is_arity_match(Arity, Arity_regexp) ->
	if 
		is_integer(Arity_regexp) -> Arity =:= Arity_regexp;
		is_list(Arity_regexp) ->
			Range = [
				case R of
					"*" -> R;
					_ ->
						case string:to_integer(R) of
							{error, _} -> 0;
							{I, _} -> I
						end
				end || R <- string:tokens(Arity_regexp, "-")],
%			Range = [R || {R, _} <- Rg],
%			io:format("Range (~p)= ~p~n", [Arity_regexp, Range]),
			case Range of
				[] -> false;
				["*"] -> true;
				[R] -> R =:= Arity;
				["*", R2] -> R2 >= Arity;
				[R1, "*"] -> Arity >= R1;
				[R1, R2] -> (Arity >= R1) and (R2 >= Arity);
				_ -> false
			end;
		true -> false
	end
.

%% @spec add_to_export(Functions::list()) -> list(tuple()) 
%% @throws nothing
%% @doc If function from Functions list has around advice then add this function to
%% -export list to allow advice around function to invoke it.
%%
add_to_export(Functions) ->
	[{list_to_atom(atom_to_list(Name) ++ "_@"), Arity} || 
			{Name, Arity, Aspects} <- Functions, 
			length([true || #aspect{advice = #advice{type = around}} <- Aspects]) > 0 ]
.

%% @spec process_forms(Forms::list(), Functions::list(T), Add_to_export::list(F)) -> Forms::list()
%%	T = {Module::atom(), Function::atom(), Aspects::list()}
%%	F = {Name::atom(), Arity::integer()}
%% @throws nothing
%% @doc Changes AF tree Forms by using Functions and Add_to_export.
%%
process_forms(Forms, Functions, Add_to_export) ->
	process_forms(Forms, Functions, Add_to_export, void, [])
.

%% @spec process_forms(Forms::list(), Functions::list(), Add_to_export::list(), Module::atom(), Result::list()) -> Forms::list()
%% @equiv weaver:process_forms/3 
%%
process_forms([], _, _, _, Result) -> lists:reverse(Result);
process_forms([{attribute, _, module, Mod} = Form | Forms], Functions, Add_to_export, _Module, Result) ->
	process_forms(Forms, Functions, Add_to_export, Mod, [Form | Result]);
process_forms([{attribute, L, export, Exp_functions} | Forms], Functions, Add_to_export, Module, Result) ->
	New_form = {attribute, L, export, lists:append(Exp_functions, Add_to_export)},
	process_forms(Forms, Functions, [], Module, [New_form | Result]);
process_forms([{function, L, Name, Arity, Fun_clauses} = Form | Forms], Functions, Add_to_export, Module, Result) ->
	New_result =
	case Find = [F || {N, A, _} = F <- Functions, (N =:= Name) and (A =:= Arity)] of
		[{_, _, []}] -> [Form | Result];
		[{_, _, Aspects}] ->
			New_name = list_to_atom(atom_to_list(Name) ++ "_@"),
			New_form = fun_proxy:target_proxy(Module, Name, Arity, New_name, L, Aspects),
			[{function, L, New_name, Arity, Fun_clauses} | [New_form | Result]];
		[] -> [Form | Result];
		_ -> 
			io:format("Error while process forms (Find = ~p)~n", [Find]),
			[Form | Result]
	end,
	process_forms(Forms, Functions, Add_to_export, Module, New_result);
process_forms([Form | Forms], Functions, Add_to_export, Module, Result) ->
	process_forms(Forms, Functions, Add_to_export, Module, [Form | Result])
.
