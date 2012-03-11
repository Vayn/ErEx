-module(flushbox).
-export([read/1, flush/0]).

%% Demonstrate how to flush mailbox

read({Db, Key}) ->
  flush(),
  Db ! {self(), {read, Key}},
  receive
    {read, R} -> {ok, R};
    {error, Reason} -> {error, Reason}
  after 1000 -> {error, timeout}
  end.

flush() ->
  receive
    {read, _} -> flush();
    {error, _} -> flush()
  after 0 -> ok
  end.
