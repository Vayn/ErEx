-module(recho).
-export([go/0, loop/0]).

%% You can use whereis(Alias) to get pid of Alias, and
%% regs() to check all registered processes.

go() ->
  register(echo, spawn(echo, loop, [])), %% register(Alias, pid)
  echo ! {self(), hello},
  receive
    {_Pid, Msg} ->
      io:format("~w~n", [Msg])
  end.

loop() ->
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      loop();
    stop ->
      true
  end.
