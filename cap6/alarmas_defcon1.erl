-module(alarmas_defcon1).
-author('manuel@altenwald.com').

-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

-record(state, {}).

init([]) ->
    {ok, #state{}};

init({[], State}) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event({defcon, 2}, State) ->
    {swap_handler, ["DEFCON2"], State, alarmas_defcon2, []};

handle_event(Event, State) ->
    io:format("DEFCON1: ~p~n", [Event]),
    {ok, State}.

terminate([Arg], State) ->
    io:format("Pasando a ~s~n", [Arg]),
    State;

terminate([], _State) ->
    io:format("Finalizado.~n", []),
    ok.
