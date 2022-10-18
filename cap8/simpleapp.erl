-module(simpleapp).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, simplesup}, simplesup, []).

stop(_State) ->
    ok.
