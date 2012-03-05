-module(simplifier).
-export([simplify/1]).
-include_lib("eunit/include/eunit.hrl").

simplify({plus, Term1, Term2}) ->
  case {simplify(Term1), simplify(Term2)} of
    {{num, 0}, {num, 0}} ->
      {num, 0};
    {{num, 0}, T2} ->
      T2;
    {T1, {num, 0}} ->
      T1;
    {T1, T2} ->
      {plus, T1, T2}
  end;

simplify({minus, Term1, Term2}) ->
  case {simplify(Term1), simplify(Term2)} of
    {{num, 0}, {num, 0}} ->
      {num, 0};
    {T1, {num, 0}} ->
      T1;
    {{num, 0}, T2} ->
      {uminus, T2};
    {T1, T2} ->
      {minus, T1, T2}
  end;

simplify({mult, Term1, Term2}) ->
  case {simplify(Term1), simplify(Term2)} of
    {{num, 1}, {num, 1}} ->
      {num, 1};
    {{num, 1}, T2} ->
      T2;
    {T1, {num, 1}} ->
      T1;
    {{num, 0}, _} ->
      {num, 0};
    {_, {num, 0}} ->
      {num, 0};
    {T1, T2} ->
      {mult, T1, T2}
  end;

simplify({divi, Term1, Term2}) ->
  case {simplify(Term1), simplify(Term2)} of
    {T1, {num, 1}} ->
      T1;
    {{num, 0}, _} ->
      {num, 0};
    {T1, T2} ->
      {divi, T1, T2}
  end;

simplify({uminus, Term}) ->
  case simplify(Term) of
    {num, 0} ->
      {num, 0};
    {uminus, T} ->
      T;
    T ->
      {uminus, T}
  end;

simplify({num, Num}) ->
  {num, Num};
simplify({var, Var}) ->
  {var, Var};

simplify({ifs, Term1, Term2, Term3}) ->
  case simplify(Term1) of
    {num, 0} ->
      simplify(Term2);
    _ ->
      simplify(Term3)
  end;

simplify({lets, Var, Term1, Term2}) ->
  {lets, Var, simplify(Term1), simplify(Term2)}.

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
  {
    lets,97,
    {uminus,{mult,{plus,{num,2},{num,5}},{num,3}}},
    {minus,{mult,{var,97},{plus,{num,8},{num,3}}},{num,2}}
  }.

format_test_() ->
  [?_assert(simplify(test_token()) == test_result())].
