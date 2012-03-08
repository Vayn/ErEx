-module('db').
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).


%% helper
merge(L, []) -> L;
merge(L, [H|T]) -> merge([H|L], T).
 
reverse([H|T], L) -> reverse(T, [H|L]);
reverse(_, L) -> L.


%% db functions 
new() -> [].
 
destroy(_) -> ok.
 
write(Key, Element, Db) ->
  [{Key, Element}|Db].
 
read(Key, Db) ->
  case Db of
    [{Key, Element}|_] -> {ok, Element};
    [_|Ts] -> read(Key, Ts);
    [] -> {error, instance}
  end.
 

delete(Key, Db) ->
  delete(Key, Db, []).

delete(Key, Db, Savers) ->
  case Db of
    [{Key, _}|Ts] -> merge(Ts, Savers);
    [H|Ts] -> delete(Key, Ts, [H|Savers]);
    [] -> Savers
  end.

match(Element, Db) ->
  match(Element, Db, []).

match(_, [], Keys) -> reverse(Keys, []);
match(Element, Db, Keys) ->
  case Db of
    [{Key, Element}|Ts] -> match(Element, Ts, [Key|Keys]);
    [_|Ts] -> match(Element, Ts, Keys)
  end.
