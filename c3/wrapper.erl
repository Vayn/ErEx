-module(wrapper).
-export([format/2]).
-include_lib("eunit/include/eunit.hrl").

%% The core is to break the string/paras down to words then assemble
%% the fraction back to new paras with new Width.

words_paras(Str) ->
  %% Split Str to para list, then split every para to words
  lists:map(fun words/1, paras(Str)).

words(Str) ->
  string:tokens(Str, " \t\n").

paras(Str) ->
  case splitwith_lb(Str) of
    {[], []} ->
      [];
    {Pre, []} ->
      [Pre];
    {Pre, Suf} ->
      [Pre] ++ paras(lists:dropwhile(fun(E)->E==$\n end, Suf))
  end.

splitwith_lb(Str) ->
  {takeuntil_lb(Str), dropuntil_lb(Str)}.

takeuntil_lb([]) ->
  [];
takeuntil_lb([$\n, $\n|_]) ->
  [];
takeuntil_lb([H|T]) ->
  [H|takeuntil_lb(T)].

dropuntil_lb([]) ->
  [];
dropuntil_lb([$\n, $\n|_]=Str) ->
  Str;
dropuntil_lb([_|T]) ->
  dropuntil_lb(T).

%% Core
format_para(Para, Width) ->
  case takewhile_len(Para, Width) of
    {[], []} -> %% Para list is empty
      [];
    {Pre, []} -> %% Para has only one element
      [Pre];
    {[], [SufH|SufT]} -> %% length(SufH) > Width
      [SufH] ++ format_para(SufT, Width);
    {Pre, Suf} -> %% Line has reached out the Width
      [Pre] ++ format_para(Suf, Width)
  end.

%% Split old para and generate new para with new Width
takewhile_len([H|T], Width) when length(H) =< Width ->
  {Pre, Suf} = takewhile_len(T, Width-length(H)-1),
  {[H|Pre], Suf};
takewhile_len(L, _) ->
  {[], L}.

assemble(Text, Width) ->
  %% Join paras
  string:join(
    lists:map(
      fun(Para) ->
        %% Join lines in a para
        string:join(
          lists:map(
            %% Join words in a line
            fun(Line) -> string:join(Line, " ") end,
            format_para(Para, Width)
          ),
          "\n"
        ) ++ "\n" %% Add tail line-break to para
      end,
      words_paras(Text)
    ),
    "\n"
  ).

format(Text, Width) ->
  io:format("~s", [assemble(Text, Width)]).

test_text() ->
  "Write a function that will\nprint this in a readable form.\n\nnext para.\n".
test_result() ->
  "Write a function that will print this in\na readable form.\n\nnext para.\n".

format_test_() ->
  [
    ?_assert(paras("This is a\ntest document\n\nThis is a document\n")
             == ["This is a\ntest document","This is a document\n"]),
    ?_assert(words("This is a\ntest document\n")
             == ["This","is","a","test","document"]),
    ?_assert(words_paras("This is a\ntest document\n\nThis is a document\n")
             == [["This","is","a","test","document"],["This","is","a","document"]]),
    ?_assert(format_para(["This","is","a","test","document"],10)
             == [["This","is","a"],["test"],["document"]]),
    ?_assert(assemble(test_text(),40) == test_result())
  ].
