-module(echo).
-export([go/0, loop/0]).

go() ->
  %% Spawn a child process
  Pid = spawn(echo, loop, []), %% spawn(module, function, [args])
  Pid ! {self(), hello}, %% {Process_identifier} = self()
  %% Receive msg from child
  receive
    {Pid, Msg} ->
      io:format("~w~n", [Msg])
  end,
  %% Why we got "stop"? Because ! clause will return value too!
  %% ** Pid1 ! Pid2 ! Pid3 Message **
  Pid ! stop.

loop() ->
  %% Receive message from Mailbox, if match the pattern
  %% send pid and msg back to parent.
  receive
    {From, Msg} ->
      From ! {self(), Msg},
      %% Every process is async, so if you need a process
      %% is alive, you can call the func recursively then
      %% the receive will let the process hang out.
      loop();
    stop ->
      true
  end.
