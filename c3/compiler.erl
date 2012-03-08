-module(compiler).
-export([compile/1]).
-include_lib("eunit/include/eunit.hrl").

% stack machine instructions:
% push i
% plus
% minus
% mult
% divi
% lets v
% var v
% jmp i
% jnz i

compile({plus, Term1, Term2}) ->
  compile(Term1) ++ compile(Term2) ++ [plus];
compile({minus, Term1, Term2}) ->
  compile(Term1) ++ compile(Term2) ++ [minus];
compile({mult, Term1, Term2}) ->
  compile(Term1) ++ compile(Term2) ++ [mult];
compile({divi, Term1, Term2}) ->
  compile(Term1) ++ compile(Term2) ++ [divi];
compile({uminus, Term}) ->
  compile(Term) ++ [uminus];
compile({num, Number}) ->
  [{push, Number}];
compile({var, Var}) ->
  [{var, Var}];
compile({ifs, Term1, Term2, Term3}) ->
  CTerm2 = compile(Term2),
  CTerm3 = compile(Term3),
  compile(Term1) ++ [{jnz, length(CTerm2)+2}] ++ CTerm2
                 ++ [{jmp, length(CTerm3)+1}] ++ CTerm3;
compile({lets, Var, Term1, Term2}) ->
  compile(Term1) ++ [{lets, Var}] ++ compile(Term2).

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

format_test_() ->
  [?_assert(compile(test_token()) == test_result())].
