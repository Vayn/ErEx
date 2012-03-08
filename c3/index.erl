-module('index').
%-export([file_to_doc/1, print_occurence/1]).
-compile([export_all]).

file_to_doc(FileName) ->
  {ok, FileBin} = file:read_file(FileName),
  FileList = binary_to_list(FileBin),
  FileLine = tokenize("\n\r",FileList), % raw document
  lists:foldl(fun(E, A) -> A ++ tokenize(" \t,./", E) end, [], FileLine). % document

tokenize(_, []) -> [];
tokenize(Delim, List) ->
  L0 = lists:dropwhile(fun(E) -> lists:member(E, Delim) end, List),
  {L1, L2} = lists:splitwith(fun(E) -> not lists:member(E, Delim) end, L0),
  [L1] ++ tokenize(Delim, L2).

print_occurence([]) -> ok;
print_occurence([{Word, Occ}|T]) ->
  io:format("~-16s~s~n",
    [Word, lists:flatten(occ_readable(occ_trans(Occ)))]),
  print_occurence(T).

occ_readable([]) -> [];
occ_readable([{S, S}]) ->
  io_lib:format("~B", [S]);
occ_readable([{S, E}]) ->
  io_lib:format("~B-~B", [S, E]);
occ_readable([{S, S}|T]) ->
  io_lib:format("~B,", [S]) ++ occ_readable(T);
occ_readable([{S, E}|T]) ->
  io_lib:format("~B-~B,", [S, E]) ++ occ_readable(T).

occ_trans([]) -> [];
occ_trans([H|T]) ->
  {End, Rest} = aggregate(H, T),
  [{H, End}|occ_trans(Rest)].

aggregate(H, [H|T]) ->
  aggregate(H, T);
aggregate(H, [H2|T]) when H+1 == H2 ->
  aggregate(H2, T);
aggregate(H, T) ->
  {H, T}.
