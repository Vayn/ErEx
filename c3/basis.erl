-module('basis').
-export([sum/1, create/1, echo/1, eecho/1]).

%% Sum from 1 to Boundary
sum(Boundary) when is_number(Boundary), Boundary > 0->
  sum(1, Boundary, 0);
sum(_) -> 0.

sum(Index, Boundary, Sum) when Index =< Boundary ->
  sum(Index+1, Boundary, Sum+Index);
sum(_, _, Sum) -> Sum.

%% Creating list
create(N) when is_number(N), N >= 0 ->
  create(N, []);
create(_) -> [].

create(N, L) when N > 0 ->
  create(N-1, [N|L]);
create(_, L) -> L.

%% Print out integers between 1 and N
echo(0) -> ok;
echo(N) ->
  io:format("Number: ~p~n", [N]),
  echo(N-1).

%% Print out even integers between 1 and N
eecho(0) -> ok;
eecho(N) when N rem 2 == 0 ->
  io:format("Number: ~p~n", [N]),
  eecho(N-2);
eecho(N) -> eecho(N-1).
