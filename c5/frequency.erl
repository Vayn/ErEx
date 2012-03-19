-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() ->
  register(frequency, spawn(frequency, init, [])).

init() ->
  Freqs = {get_freqs(), []},
  loop(Freqs).

get_freqs() -> [130,131,132,133,134,135,136,137].

loop(Freqs) ->
  receive
    {request, Pid, allocate} ->
      {NewFreqs, Re} = allocate(Freqs, Pid),
      reply(Pid, Re),
      loop(NewFreqs);
    {request, Pid, {deallocate, Freq}} ->
      NewFreqs = deallocate(Freqs, Freq),
      reply(Pid, ok),
      loop(NewFreqs);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

reply(Pid, Re) ->
  Pid ! {reply, Re}.

%% helpers
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
               % lists:keydelete(elem, pos, list)
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.


%% Client

stop () -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Msg) ->
  frequency ! {request, self(), Msg},
  receive
    {reply, Re} -> Re
  end.
