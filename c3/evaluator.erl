-module(evaluator).
-export([eval/1]).
-include_lib("eunit/include/eunit.hrl").

eval(Term) ->
  eval(Term, []).

eval({plus, Term1, Term2}, Env) ->
  eval(Term1, Env) + eval(Term2, Env);
eval({minus, Term1, Term2}, Env) ->
  eval(Term1, Env) - eval(Term2, Env);
eval({mult, Term1, Term2}, Env) ->
  eval(Term1, Env) * eval(Term2, Env);
eval({divi, Term1, Term2}, Env) ->
  eval(Term1, Env) / eval(Term2, Env);
eval({uminus, Term}, Env) ->
  -eval(Term, Env);
eval({num, Num}, _Env) ->
  Num;
eval({var, Var}, [{Var, Val}|_]) ->
  Val;
eval({var, Var}, [_|Rest]) ->
  eval({var, Var}, Rest);
eval({ifs, Term1, Term2, Term3}, Env) ->
  case eval(Term1, Env) of
    0 ->
      eval(Term2, Env);
    _ ->
      eval(Term3, Env)
  end;
eval({lets, Var, Term1, Term2}, Env) ->
  NewEnv = [{Var, eval(Term1)}|Env],
  eval(Term2, NewEnv).

test_token() ->
  %% parser:parse("let a = if 0 then ~((2+5)*3) else 9 in ((a*(8+3))-2)").
  {
    lets,97,
    {ifs,{num,0},
         {uminus,{mult,{plus,{num,2},{num,5}},{num,3}}},
         {num,9}},
    {minus,{mult,{var,97},{plus,{num,8},{num,3}}},{num,2}}
  }.

test_result() ->
  %% simulator:sim(compiler:compile(test_token()))
  -233.

format_test_() ->
  [?_assert(eval(test_token()) == test_result())].
