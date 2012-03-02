-module(logic_op).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).
 
 
b_not(true) ->
  false;
b_not(false) ->
  true.

b_and(true, true) ->
  true;
b_and(_, _) ->
  false.

%% http://en.wikipedia.org/wiki/De_Morgan%27s_laws
b_or(X, Y) ->
  b_not(b_and(b_not(X), b_not(Y))).

b_nand(X, Y) ->
  b_not(b_and(X, Y)).
