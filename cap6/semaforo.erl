-module(semaforo).
-author('manuel@altenwald.com').

-behaviour(gen_statem).

-export([
    start_link/2,
    stop/0,
    ver_semaforo/0
]).

-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3,
    code_change/4
]).

-define(TIEMPO_EN_AMBAR, 1000). %% 1 segundo

-record(state, {
    tiempo_verde :: pos_integer(),
    tiempo_rojo :: pos_integer()
}).

start_link(TiempoVerde, TiempoRojo) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE,
                          [TiempoVerde, TiempoRojo], []).

stop() ->
    gen_statem:stop(?MODULE).

ver_semaforo() ->
    gen_statem:call(?MODULE, ver_semaforo).

callback_mode() ->
    handle_event_function.

init([TiempoVerde, TiempoRojo]) ->
    {ok, rojo, #state{
        tiempo_verde = TiempoVerde,
        tiempo_rojo = TiempoRojo
    }, [{state_timeout, TiempoRojo, {cambia, verde}}]}.

handle_event(state_timeout, Event, rojo, State) ->
    io:format("* cambia a ambar~n", []),
    {next_state, ambar, State,
     [{state_timeout, ?TIEMPO_EN_AMBAR, Event}]};

handle_event(state_timeout, {cambia, verde}, ambar, State) ->
    io:format("* cambia a verde~n", []),
    {next_state, verde, State,
     [{state_timeout, State#state.tiempo_verde, {cambia, rojo}}]};
handle_event(state_timeout, {cambia, rojo}, ambar, State) ->
    io:format("* cambia a rojo~n", []),
    {next_state, rojo, State,
     [{state_timeout, State#state.tiempo_rojo, {cambia, verde}}]};

handle_event(state_timeout, Event, verde, State) ->
    io:format("* cambia a ambar~n", []),
    {next_state, ambar, State,
     [{state_timeout, ?TIEMPO_EN_AMBAR, Event}]};

handle_event({call, From}, ver_semaforo, StateName, _StateData) ->
    {keep_state_and_data,
     [{reply, From, StateName}]}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
