-module(parser).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").

% rule:
% term = "(" expr ")"     -> Exp
%      | "~" term         -> {uminus, term}
%      | num              -> {num, Number}
%      | var              -> {var, Var}
%      | "if " term " then " term " else " term
%                         -> {ifs, Term1, Term2, Term3}
%      | "let " var " = " term " in " term
%                         -> {lets, Var, Term1, Term2}
% expr = term "+" term    -> {plus, Term1, Term2}
%      | term "-" term    -> {minus, Term1, Term2}
%      | term "*" term    -> {mult, Term1, Term2}
%      | term "/" term    -> {divi, Term1, Term2}
% num = [0-9]
% var = [a-z]

parse(String) ->
  {Result, []} = term(String),
  Result.

term([$(|Xs]) ->
  {Result, [$)|Ts]} = expr(Xs),
  {Result, Ts};
term([$~|Xs]) ->
  {Term, Ts} = term(Xs),
  {{uminus, Term}, Ts};
term([$i, $f, $ |Xs]) ->
  {Term1, [$ , $t, $h, $e, $n, $ |Ts1]} = term(Xs),
  {Term2, [$ , $e, $l, $s, $e, $ |Ts2]} = term(Ts1),
  {Term3, Ts3} = term(Ts2),
  {{ifs, Term1, Term2, Term3}, Ts3};
term([$l, $e, $t, $ |Xs]) ->
  {Var, [$ , $=, $ |Ts1]} = {hd(Xs), tl(Xs)}, % [H|T] = Xs => {H, T}
  {Term1, [$ , $i, $n, $ |Ts2]} = term(Ts1),
  {Term2, Ts3} = term(Ts2),
  {{lets, Var, Term1, Term2}, Ts3};
term([Num|Xs]) when Num >= $0, Num =< $9 ->
  {{num, Num-$0}, Xs};
term([Var|Xs]) when Var >= $a, Var =< $z ->
  {{var, Var}, Xs}.

expr(Rest) ->
  {Term1, [Opc|Xs]} = term(Rest),
  Op = case Opc of
    $+ -> plus;
    $- -> minus;
    $* -> mult;
    $/ -> divi
  end,
  {Term2, Ts} = term(Xs),
  {{Op, Term1, Term2}, Ts}.

format_test_() ->
  [
    ?_assert(parse("if 0 then (1+1) else (2*3)")
      == {ifs,{num,0},{plus,{num,1},{num,1}},{mult,{num,2},{num,3}}}),
    ?_assert(parse("~((1+1)/2)")
      == {uminus,{divi,{plus,{num,1},{num,1}},{num,2}}}),
    ?_assert(parse("let a = 2 in ~((a*2)+5)")
      == {lets,97,{num,2},{uminus,{plus,{mult,{var,97},{num,2}},{num,5}}}})
  ].
