-module('manipu').
-export([filter/2, concat/1, flatten/1]).
 
 
filter(L, Int) when is_integer(Int) ->
  filter(L, Int, []);
filter(L, _) -> L.

filter([], _, L) -> reverse(L);
filter([H|T], Int, L) ->
  if
    H =< Int -> filter(T, Int, [H|L]);
    true -> filter(T, Int, L)
  end.
 
reverse(L) -> reverse(L, []).

reverse([H|T], L) -> reverse(T, [H|L]);
reverse(_, L) -> L.
 
concat([X|Xs]) -> concat(X, Xs, []).

concat([X|Xs], T, L) -> concat(Xs, T, [X|L]);
concat([], [X|Xs], L) -> concat(X, Xs, L);
concat([], [], L) -> reverse(L).
 
 
flatten(X) -> reverse(flatten(X, [])).

flatten([], Acc) -> Acc;
flatten([H|T], Acc) ->
  case H of
    [_|_] -> flatten(T, flatten(H, Acc));
    [] -> flatten(T, Acc);
    _ -> flatten(T, [H|Acc])
  end.
