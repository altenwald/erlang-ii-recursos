-module(task).

-export([start_link/1]).

start_link({M, F, A}) ->
    PID = spawn_link(M, F, A),
    {ok, PID}.
