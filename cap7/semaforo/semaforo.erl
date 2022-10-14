-module(semaforo).
-author('manuel@altenwald.com').

-behaviour(gen_statem).

-export([
    start_link/1,
    start_link/2,
    stop/1,
    ver_semaforo/1
]).

-export([
    callback_mode/0,
    init/1,
    handle_event/4
]).

-define(TIEMPO_EN_AMBAR, 3_000). %% 3 segundos
-define(TIEMPO_EN_ROJO, 5_000). %% 3 segundos
-define(TIEMPO_EN_VERDE, 5_000). %% 5 segundos

-record(state, {
    tiempo_verde :: pos_integer(),
    tiempo_rojo :: pos_integer(),
    links = [] :: [atom()],
    name :: atom()
}).

start_link(Name) ->
    gen_statem:start_link({local, Name}, ?MODULE,
                          [Name], []).

start_link(Name, Links) ->
    gen_statem:start_link({local, Name}, ?MODULE,
                          [Name, Links], []).

stop(Name) ->
    gen_statem:stop(Name).

ver_semaforo(Name) ->
    gen_statem:call(Name, ver_semaforo).

callback_mode() ->
    handle_event_function.

init([Name]) ->
    init([Name, []]);

init([Name, Links]) when is_list(Links) ->
    StateData = #state{
        tiempo_verde = ?TIEMPO_EN_VERDE,
        tiempo_rojo = TRojo = ?TIEMPO_EN_ROJO,
        links = Links,
        name = Name
    },
    case Links of
        [] ->
            imprime_estado(inicio, StateData),
            imprime_estado(rojo, StateData),
            InitialState = rojo,
            Actions = [];
        _ ->
            imprime_estado(inicio, StateData),
            InitialState = verde,
            Actions = [
                {state_timeout, TRojo, {cambia, rojo}}
            ]
    end,
    {ok, InitialState, StateData, Actions}.

handle_event(state_timeout, Event, rojo, State) ->
    imprime_estado(ambar, State),
    lists:foreach(fun(Link) ->
        case Event of
            {cambia, rojo} ->
                gen_statem:cast(Link, {cambia, verde});
            {cambia, verde} ->
                gen_statem:cast(Link, {cambia, rojo})
        end
    end, State#state.links),
    {next_state, ambar, State,
     [{state_timeout, ?TIEMPO_EN_AMBAR, Event}]};

handle_event(cast, {cambia, _} = Event, rojo, State) ->
    imprime_estado(ambar, State),
    {next_state, ambar, State,
     [{state_timeout, ?TIEMPO_EN_AMBAR, Event}]};

handle_event(cast, {cambia, _}, ambar, _State) ->
    {keep_state_and_data, [postpone]};

handle_event(state_timeout, {cambia, verde}, ambar, State) ->
    imprime_estado(verde, State),
    {next_state, verde, State,
     [{state_timeout, State#state.tiempo_verde, {cambia, rojo}}]};

handle_event(state_timeout, {cambia, rojo}, ambar, State) ->
    imprime_estado(rojo, State),
    {next_state, rojo, State,
     [{state_timeout, State#state.tiempo_rojo, {cambia, verde}}]};

handle_event(state_timeout, Event, verde, State) ->
    imprime_estado(ambar, State),
    lists:foreach(fun(Link) ->
        case Event of
            {cambia, rojo} ->
                gen_statem:cast(Link, {cambia, verde});
            {cambia, verde} ->
                gen_statem:cast(Link, {cambia, rojo})
        end
    end, State#state.links),
    {next_state, ambar, State,
     [{state_timeout, ?TIEMPO_EN_AMBAR, Event}]};

handle_event(cast, {cambia, _} = Event, verde, State) ->
    imprime_estado(ambar, State),
    {next_state, ambar, State,
     [{state_timeout, ?TIEMPO_EN_AMBAR, Event}]};

handle_event({call, From}, ver_semaforo, StateName, _StateData) ->
    {keep_state_and_data,
     [{reply, From, StateName}]}.

imprime_estado(StateName, #state{name = Name}) ->
    io:format("~s => ~s~n", [Name, StateName]).
