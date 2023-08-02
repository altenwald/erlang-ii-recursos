-module(elevator_app).
-author('manuel@altenwald.com').

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    elevator_sup:start_link().

stop(_State) ->
    ok.
