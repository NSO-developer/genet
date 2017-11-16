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

%% @since 2011-06-24
%% @copyright 2011 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc
%% The module generates proxy for target function in form of AF tree.
-module(fun_proxy).

%%
%% Include files
%%
-include("aop.hrl").

%%
%% Import modules
%%

%%
%% Exported Functions
%%
-export([target_proxy/6]).

%%
%% API Functions
%%

%% @spec target_proxy(Module, Name, Arity, Hidden_name, Line, Aspects) -> term()
%%      Module = atom()
%%      Name = atom()
%%      Arity = atom()
%%      Hidden_name = atom()
%%      Line = integer()
%%      Aspects = list(#aspect{})
%%
%% @throws nothing
%% @doc The function generates AF tree that corresponded with source code like this:
%% name(P1,P2,..) ->
%%      T = [?MODULE, name_@, [P1,P2,..]],
%%      advice1:before1(T),
%%      advice2:before2(T),
%%              try
%%                      R = erlang:apply(advice, arround_1, [advice, arround_2, T]),
%%                      advice1:after_r1(T,R),
%%                      advice2:after_r2(T,R),
%%                      R
%%              catch
%%                      Ex:Rz ->
%%                              advice1:after_e1(T,{Ex,Rz}),
%%                              advice2:after_e2(T,{Ex,Rz})
%%              after
%%                      advice1:after_f1(T),
%%                      advice2:after_f2(T)
%%              end
%% .
%%
target_proxy(Module, Name, Arity, Hidden_name, Line, Aspects) ->
    {function, Line, Name, Arity,
     [{
        clause, Line, args_list(Arity, Line),
        [],
        [
         set_t(Module, Hidden_name, Arity, Line) |
         body(Module, Name, Arity, Hidden_name, Line, Aspects)
        ]
      }]
    }
        .

%%
%% Local Functions
%%

%% @spec call_target(Name, Arity, Line) -> term()
%%      Name = atom()
%%      Arity = integer()
%%      Line = integer()
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      name(P1,P2,..)
%%
call_target(Name, Arity, Line) ->
    {
      call, Line,
      {atom, Line, Name},
      args_list(Arity, Line)
    }
        .

%% @spec call_target_r(Name, Arity, Line) -> term()
%%      Name = atom()
%%      Arity = integer()
%%      Line = integer()
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      R = name(P1,P2,..)
%%
call_target_r(Name, Arity, Line) ->
    {
      match,Line, {var,Line,'R'},
      call_target(Name, Arity, Line)
    }
        .

%% @spec call_generic_advice(Line, Aspect, Param_list) -> term()
%%      Line = integer()
%%      Aspect = #aspect{}
%%      Param_list = list(atom)
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      advice_module:advice_function(P1,P2,..)
%%
call_generic_advice(Line, Aspect, Param_list) ->
    Advice = Aspect#aspect.advice,
    {
      call, Line,
      {
        remote, Line,
        {atom, Line, Advice#advice.module},
        {atom, Line, Advice#advice.function}
      },
      [advice_argument(Line, Arg) || Arg <- Advice#advice.arguments]
      ++ Param_list  % list of parameters of advice function
    }.

advice_argument(Line, A) when is_atom(A) ->
    {atom, Line, A};
advice_argument(Line, I) when is_integer(I) ->
    {integer, Line, I}.

%% @spec call_advice(Line, Aspect) -> term()
%%      Line = integer()
%%      Aspect = #aspect{}
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      advice_module:advice_function(T)
%%
call_advice(Line, Aspect) ->
    call_generic_advice(Line, Aspect, [{var, Line, 'T'}])
        .

%% @spec call_after_advice(Line, Aspect) -> term()
%%      Line = integer()
%%      Aspect = #aspect{}
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      advice_module:advice_function(T, R)
%%
call_after_advice(Line, Aspect) ->
    call_generic_advice(Line, Aspect, [{var, Line, 'T'}, {var, Line, 'R'}])
        .

%% @spec call_after_thr_advice(Line, Aspect) -> term()
%%      Line = integer()
%%      Aspect = #aspect{}
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      advice_module:advice_function(T, {Ex, Rz})
%% Ex - exception, Rz - error reason.
%%
call_after_thr_advice(Line, Aspect) ->
    call_generic_advice(
      Line,
      Aspect,
      [{var, Line, 'T'},{tuple, Line,[{var,Line,'Ex'},{var,Line,'Rz'}]}]
     )
        .

%% @spec call_around_advices(Name, Arity, Line, Aspects) -> term()
%%      Name = atom()
%%      Arity = atom()
%%      Line = integer()
%%      Aspects = list(#aspect{})
%%
%% @throws nothing
%% @doc the function generate part of AF tree corresponded with:
%%      R = erlang:apply(around_params(Line, Aspects))
%%          or
%%      R = name(P1,P2,..), if Aspects = []
%% The function arguments are
%%      Name - target function name
%%      Arity - target function arity
%%      Line - current line of source file
%%      Aspects - list of around aspects for the target function
%%
call_around_advices(Name, Arity, Line, []) ->
    call_target_r(Name, Arity, Line);
call_around_advices(_Name, _Arity, Line, Aspects) ->
    {
      match, Line, {var, Line, 'R'},
                                                % call apply
      {
        call, Line,
        {remote, Line, {atom, Line, erlang}, {atom, Line, apply}},
                                                % call's arguments:
        around_params(Line, Aspects)
      }
    }
        .

around_params(Line, Aspects) ->
    around_params_rcur(Line, Aspects, {var,Line,'T'})
        .

around_params_rcur(Line, [Aspect | []], Result) ->
    [
     {atom,Line,(Aspect#aspect.advice)#advice.module}, {atom,Line,(Aspect#aspect.advice)#advice.function}, Result
    ];
around_params_rcur(Line, [Aspect | Aspects], Result) ->
    R = {cons,Line,{atom,Line, (Aspect#aspect.advice)#advice.module},
         {cons,Line,{atom,Line, (Aspect#aspect.advice)#advice.function},
          {cons,Line, Result, {nil,Line}}
         }
        },
    around_params_rcur(Line, Aspects, R)
        .

args_list(N, Line) -> args_list(N, Line, []).

args_list(0, _, Result) -> Result;
args_list(N, Line, Result) ->
    args_list(N-1, Line, [{var, Line, list_to_atom("P" ++ integer_to_list(N))} | Result])
        .

args_to_list(N, Line) -> args_to_list(N, Line, {nil, Line}).

args_to_list(0, _, Result) -> Result;
args_to_list(N, Line, Result) ->
    args_to_list(N-1, Line,
                 {cons, Line, {var, Line, list_to_atom("P" ++ integer_to_list(N))}, Result})
        .

set_t(Module, Name, Arity, Line) ->
    {match, Line, {var, Line, 'T'},
     {cons, Line, {atom, Line, Module},
      {cons, Line, {atom, Line, Name},
       {cons, Line, {integer, Line, Line},
        {cons, Line, args_to_list(Arity, Line),
         {nil, Line}
        }
       }
      }
     }
    }
        .

body(Module, Name, Arity, Hidden_name, Line, Aspects) ->
    Type_filter = fun(T) -> fun(#aspect{advice = #advice{type = Type}}) when T == Type -> true;
                               (_) -> false
                            end end,
    After_Aspects = lists:filter(Type_filter(after_return), Aspects),
    After_thr_Aspects = lists:filter(Type_filter(after_throw), Aspects),
    After_fin_Aspects = lists:filter(Type_filter(after_final), Aspects),
    Around_Aspects = lists:filter(Type_filter(around), Aspects),
    Before_Aspects = lists:filter(Type_filter(before), Aspects),
    body_proxy_func(
      Module, Name, Arity, Hidden_name, Line,
      {After_thr_Aspects, After_fin_Aspects, After_Aspects, Around_Aspects, Before_Aspects}
     )
        .

body_proxy_func(Module, Name, Arity, Hidden_name, Line, {[], [], After_Aspects, Around_Aspects, Before_Aspects}) ->

    R1 = body_gen_aft(Module, Name, Arity, Line, After_Aspects, [{var,Line,'R'}]),
    R2 = [call_around_advices(Hidden_name, Arity, Line, Around_Aspects) | R1],
    body_gen_bef(Module, Name, Arity, Line, Before_Aspects, R2)
        ;
body_proxy_func(Module, Name, Arity, Hidden_name, Line,
                {After_thr_Aspects, After_fin_Aspects, After_Aspects, Around_Aspects, Before_Aspects}) ->
    B = body_gen_try(Module, Name, Arity, Hidden_name, Line,
                     {After_thr_Aspects, After_fin_Aspects, After_Aspects, Around_Aspects}),
    body_gen_bef(Module, Name, Arity, Line, Before_Aspects, [B])
        .

body_try(Module, Name, Arity, Hidden_name, Line,{After_Aspects, Around_Aspects}) ->
    R1 = body_gen_aft(Module, Name, Arity, Line, After_Aspects, [{var,Line,'R'}]),
    [call_around_advices(Hidden_name, Arity, Line, Around_Aspects) | R1]
        .

body_gen_try(Module, Name, Arity, Hidden_name, Line,
             {After_thr_Aspects, After_fin_Aspects, After_Aspects, Around_Aspects}) ->
    {'try',Line,
     body_try(Module, Name, Arity, Hidden_name, Line, {After_Aspects, Around_Aspects}),
     [],
                                                % catch block
     [{
        clause,Line,
        [{
           tuple,Line,
           [{var,Line,'Ex'},{var,Line,'Rz'},{var,Line,'_'}]
         }],
        [],
        body_gen_aft_thr(Module, Name, Arity, Line, After_thr_Aspects, [])
      }],
                                                % after block
     body_gen_bef(Module, Name, Arity, Line, After_fin_Aspects, [])
    }
        .

body_gen_aft(_Module, _Name, _Arity, _Line, [], Result) ->
    Result;
body_gen_aft(Module, Name, Arity, Line, [Aspect | Aspects], Result) ->
    body_gen_aft(
      Module, Name, Arity, Line, Aspects,
      [call_after_advice(Line, Aspect) | Result]
     )
        .

body_gen_aft_thr(_Module, _Name, _Arity, _Line, [], Result) ->
    Result;
body_gen_aft_thr(Module, Name, Arity, Line, [Aspect | Aspects], Result) ->
    body_gen_aft_thr(Module, Name, Arity, Line, Aspects,
                     [call_after_thr_advice(Line, Aspect) | Result])
        .

body_gen_bef(_Module, _Name, _Arity, _Line, [], Result) -> Result;
body_gen_bef(Module, Name, Arity, Line, [Aspect | Aspects], Result) ->
    body_gen_bef(Module, Name, Arity, Line, Aspects,
                 [call_advice(Line, Aspect) | Result])
        .
