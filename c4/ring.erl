-module(ring).
-export([start/3, loop/1]).

%% Create N process,
%% send M Messages
start(M, N, Msg) ->
  First = make_ring(N, self()),
  send_ntimes(M, Msg, First),
  First ! stop,
  ok.

make_ring(1, Pid) ->
  Pid;
make_ring(N, Pid) ->
  spawn(?MODULE, loop, [make_ring(N-1, Pid)]).

loop(NextProc) ->
  receive
    {message, Content} = Msg ->
      %io:format("Proc ~p receives '~p'.~n", [self(), Content]),
      NextProc ! Msg,
      loop(NextProc);
    stop ->
      %io:format("Proce ~p stops.~n", [self()]),
      NextProc ! stop,
      ok
  end.

send_ntimes(0, _, _) ->
  ok;
send_ntimes(M, Msg, Pid) ->
  Pid ! {message, Msg},
  receive
    {message, Content} ->
      %io:format("Proc ~p receives '~p'.~n", [self(), Content]),
      ok
  end,
  send_ntimes(M-1, Msg, Pid).
