-module(pprint).
-export([print/1]).
-include_lib("eunit/include/eunit.hrl").

print({plus, Term1, Term2}) ->
  "(" ++ print(Term1) ++ "+" ++ print(Term2) ++ ")";
print({minus, Term1, Term2}) ->
  "(" ++ print(Term1) ++ "-" ++ print(Term2) ++ ")";
print({mult, Term1, Term2}) ->
  "(" ++ print(Term1) ++ "*" ++ print(Term2) ++ ")";
print({divi, Term1, Term2}) ->
  "(" ++ print(Term1) ++ "/" ++ print(Term2) ++ ")";
print({uminus, Term}) ->
  "~" ++ print(Term);
print({ifs, Term1, Term2, Term3}) ->
  "if " ++ print(Term1) ++ " then " ++ print(Term2) ++
                           " else " ++ print(Term3);
print({lets, Var, Term1, Term2}) ->
  "let " ++ [Var] ++ " = " ++ print(Term1) ++ " in " ++ print(Term2);
print({var, Var}) ->
  [Var];
print({num, Num}) ->
  [Num+$0].

test_string() ->
  "let a = if 0 then ~((2+5)*3) else 9 in ((a*(8+3))-2)".

test_token() ->
  %% parser:parse(test_string).
  {
    lets,97,
    {ifs,{num,0},
         {uminus,{mult,{plus,{num,2},{num,5}},{num,3}}},
         {num,9}},
    {minus,{mult,{var,97},{plus,{num,8},{num,3}}},{num,2}}
  }.

format_test_() ->
  [?_assert(print(test_token()) == test_string())].
