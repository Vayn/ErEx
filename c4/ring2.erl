-module(ring2).
-export([start/3]).
-export([init/1, init/2]).

start(M, N, Msg) ->
  First = spawn(?MODULE, init, [N]),
  First ! {msg, Msg, M},
  ok.

init(N) ->
  %% 1st spawned proc
  NextProc = spawn(?MODULE, init, [N-1, self()]),
  loop(NextProc).

init(0, First) ->
  loop(First);
init(N, First) ->
  NextProc = spawn(?MODULE, init, [N-1, First]),
  loop(NextProc).

loop(NextProc) ->
  receive
    {msg, _, 0} -> ok;
    {msg, Msg, M} ->
      io:format("Proc ~p receive ~p.~n", [self(), Msg]),
      NextProc ! {msg, Msg, M-1},
      loop(NextProc)
  end.
