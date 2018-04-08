-module(logs_consola).
-author('manuel@altenwald.com').

-behaviour(gen_event).

-export([
    add_handler/0,
    delete_handler/0
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

add_handler() ->
    gen_event:add_handler(logs, ?MODULE, []).

delete_handler() ->
    gen_event:delete_handler(logs, ?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, State) ->
    io:format("~p~n", [Event]),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate([], _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("consola no cambia!~n", []),
    {ok, State}.
