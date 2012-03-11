-module(benchmark).
-export([start/1, start_proc/2]).

%% Use timer:tc(Module, Function, [Args]) to run this benchmark

start(Num) ->
  start_proc(Num, self()).

start_proc(0, Pid) ->
  Pid ! ok;
start_proc(Num, Pid) ->
  NPid = spawn(?MODULE, start_proc, [Num-1, Pid]),
  NPid ! ok,
  receive ok -> ok end.
