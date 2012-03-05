-module(simulator).
-export([sim/1]).
-include_lib("eunit/include/eunit.hrl").

sim(Insts) ->
  sim(Insts, [], []).

sim([], [H|_], _) ->
  H;
sim([{push, I}|T], Stack, Env) ->
  sim(T, [I|Stack], Env);
sim([{var, Var}|T], Stack, Env) ->
  Val = search_env(Var, Env),
  sim(T, [Val|Stack], Env);
sim([plus|T], [T1, T2|Stack], Env) ->
  sim(T, [T2+T1|Stack], Env);
sim([minus|T], [T1, T2|Stack], Env) ->
  sim(T, [T2-T1|Stack], Env);
sim([mult|T], [T1, T2|Stack], Env) ->
  sim(T, [T2*T1|Stack], Env);
sim([divi|T], [T1, T2|Stack], Env) ->
  sim(T, [T2/T1|Stack], Env);
sim([uminus|T], [Term|Stack], Env) ->
  sim(T, [-Term|Stack], Env);
sim([{lets, Var}|T], [Val|Stack], Env) ->
  sim(T, Stack, [{Var, Val}|Env]);
sim([{jnz, _}|T], [0|Stack], Env) ->
  %% When the value of Term1 in ifs cause is 0,
  %% we can simply execute next instruction Term2.
  sim(T, Stack, Env);
sim([{jnz, Dest}|T], [_|Stack], Env) ->
  sim(skip(Dest-1, T), Stack, Env);
sim([{jmp, Dest}|T], Stack, Env) ->
  sim(skip(Dest-1, T), Stack, Env).

search_env(Var, [{Var, Val}|_]) ->
  Val;
search_env(Var, [_|T]) ->
  search_env(Var, T).

skip(0, Insts) ->
  Insts;
skip(N, [_|T]) ->
  skip(N-1, T).

test_instruction() ->
  %% Tk = parser:parse("let a = if 0 then ~((2+5)*3) else 9 in ((a*(8+3))-2)").
  %% compiler:compile(Tk).
  [
    {push,0},
    {jnz,8},
    {push,2},
    {push,5},
    plus,
    {push,3},
    mult,
    uminus,
    {jmp,2},
    {push,9},
    {lets,97},
    {var,97},
    {push,8},
    {push,3},
    plus,
    mult,
    {push,2},
    minus
  ].

test_result() ->
  %% evaluator:eval(Tk).
  -233.

format_test_() ->
  [?_assert(sim(test_instruction()) == test_result())].
