-module(sort).
-export([qsort1/1, qsort2/1, qsort3/1, msort_lte/1, msort_gte/1]).


%% quick sort
qsort1([]) -> [];
qsort1([H|T]) ->
  qsort1([X || X <- T, X < H]) ++ [H] ++ qsort1([X || X <- T, X >= H]).

qsort2([]) -> [];
qsort2(X) ->
  {L, E, G} = part(X),
  qsort2(L) ++ E ++ qsort2(G).

part([H|T]) ->
  part(H, T, {[], [H], []}). 

part(_, [], {L, E, G}) ->
  {L, E, G};
part(Pivot, [H|T], {L, E, G}) ->
  if
    H < Pivot -> part(Pivot, T, {[H|L], E, G});
    H > Pivot -> part(Pivot, T, {L, E, [H|G]});
    true -> part(Pivot, T, {L, [H|E], G})
  end.


qsort3([]) -> [];
qsort3(L) -> qsort3(L, []).

qsort3([], Acc) -> Acc;
qsort3([H|T], Acc) ->
  part3(H, T, {[], [H], []}, Acc).

part3(_, [], {L, E, G}, Acc) ->
  qsort3(L, (E ++ qsort3(G, Acc)));
part3(Pivot, [H|T], {L, E, G}, Acc) ->
  if
    H < Pivot -> part3(Pivot, T, {[H|L], E, G}, Acc);
    H > Pivot -> part3(Pivot, T, {L, E, [H|G]}, Acc);
    true -> part3(Pivot, T, {L, [H|E], G}, Acc)
  end.

%% merge sort
split([]) -> [];
split(L) -> split(L, L, []).

split([], Ls1, Ls2) ->
  {Ls1, Ls2};
split([_], Ls1, Ls2) ->
  {Ls1, Ls2};
split([_, _|Ts], [H|T], Ls2) ->
  split(Ts, T, [H|Ls2]).

msort(_, []) -> [];
msort(_, [H]) -> [H];
msort(Cmp, L) ->
  {Half1, Half2} = split(L),
  merge(Cmp, msort(Cmp, Half1), msort(Cmp, Half2)).

merge(_, Ls1, []) -> Ls1;
merge(_, [], Ls2 ) -> Ls2;
merge(Cmp, [H1|T1], [H2|T2]) ->
  case Cmp(H1, H2) of
    true -> [H1|merge(Cmp, T1, [H2|T2])];
    false -> [H2|merge(Cmp, [H1|T1], T2)]
  end.

lte(X, Y) -> X =< Y.
gte(X, Y) -> X >= Y.

msort_lte(Ls) -> msort(fun lte/2, Ls).
msort_gte(Ls) -> msort(fun gte/2, Ls).
