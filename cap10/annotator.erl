-module(annotator).
-author('manuel@altenwald.com').

-export([log/2]).

log(#{level := Level, msg := Msg}, Config) ->
    io:format("~b ~p ~p~n", [logger:timestamp(), Level, Msg]),
    io:format("    ~p~n", [Config]),
    ok.
