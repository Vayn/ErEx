-module(echo).
-export([start/0, print/1, stop/0]).

start() ->
  register(?MODULE, spawn(fun loop/0)),
  ok.

print(Term) ->
  ?MODULE ! {print, Term},
  ok.

stop() ->
  ?MODULE ! stop,
  ok.

loop() ->
  receive
    {print, Term} ->
      io:format("Echo Server:~p~n", [Term]),
      loop();
    stop ->
      ok;
    _ ->
      {error, unknown_message}
  end.
