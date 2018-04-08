-module(semaforo).
-author('manuel@altenwald.com').
-vsn(3).

-behaviour(gen_fsm).

-export([
    start_link/1,
    start_link/2,
    stop/1,
    ver_semaforo/1
]).

-export([
    init/1,
    rojo/2,
    ambar/2,
    verde/2,
    handle_sync_event/4,
    handle_event/3,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-define(TIEMPO_EN_AMBAR, 5000). %% 1 segundo
-define(TIEMPO_EN_ROJO, 5000). %% 1 segundo
-define(TIMEPO_EN_VERDE, 8000). %% 2 segundos

-record(state, {
    tiempo_verde :: pos_integer(),
    tiempo_rojo :: pos_integer(),
    siguiente :: verde | rojo,
    links = [] :: [atom()],
    name :: atom()
}).

start_link(Name) ->
    gen_fsm:start_link({local, Name}, ?MODULE,
                       [Name], []).

start_link(Name, Links) ->
    gen_fsm:start_link({local, Name}, ?MODULE,
                       [Name, Links], []).

stop(Name) ->
    gen_fsm:send_all_state_event(Name, stop).

ver_semaforo(Name) ->
    gen_fsm:sync_send_all_state_event(Name, ver_semaforo).

init([Name]) ->
    State = #state{
        tiempo_verde = ?TIMEPO_EN_VERDE,
        tiempo_rojo = ?TIEMPO_EN_ROJO,
        name = Name
    },
    imprime_estado(inicio, State),
    imprime_estado(rojo, State),
    {ok, rojo, State};
init([Name, Links]) ->
    State = #state{
        tiempo_verde = ?TIMEPO_EN_VERDE,
        tiempo_rojo = ?TIEMPO_EN_ROJO,
        links = Links,
        name = Name
    },
    imprime_estado(inicio, State),
    imprime_estado(verde, State),
    gen_fsm:send_event_after(?TIEMPO_EN_ROJO, timeout),
    {ok, verde, State}.

rojo(Event, State) when Event =:= timeout orelse
                        Event =:= change ->
    gen_fsm:send_event_after(?TIEMPO_EN_AMBAR, timeout),
    imprime_estado(ambar, State),
    {next_state, ambar, State#state{siguiente = verde}}.

ambar(change, State) ->
    {next_state, ambar, State};
ambar(timeout, #state{siguiente = verde} = State) ->
    gen_fsm:send_event_after(State#state.tiempo_verde, timeout),
    lists:foreach(fun(Link) ->
        gen_fsm:send_event(Link, change)
    end, State#state.links),
    imprime_estado(verde, State),
    {next_state, verde, State};
ambar(timeout, #state{siguiente = rojo} = State) ->
    gen_fsm:send_event_after(State#state.tiempo_rojo, timeout),
    lists:foreach(fun(Link) ->
        gen_fsm:send_event(Link, change)
    end, State#state.links),
    imprime_estado(rojo, State),
    {next_state, rojo, State}.

verde(Event, State) when Event =:= timeout orelse
                         Event =:= change ->
    gen_fsm:send_event_after(?TIEMPO_EN_AMBAR, timeout),
    imprime_estado(ambar, State),
    {next_state, ambar, State#state{siguiente = rojo}}.

handle_sync_event(ver_semaforo, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, rojo,
            #state{tiempo_rojo = TiempoRojo} = State,
            _Extra) ->
    gen_fsm:send_event_after(TiempoRojo, timeout),
    {ok, State};
code_change(_OldVsn, verde,
            #state{tiempo_rojo = TiempoVerde} = State,
            _Extra) ->
    gen_fsm:send_event_after(TiempoVerde, timeout),
    {ok, State};
code_change(_OldVsn, ambar, State, _Extra) ->
    gen_fsm:send_event_after(?TIEMPO_EN_AMBAR, timeout),
    {ok, State}.

imprime_estado(StateName, #state{name = Name}) ->
    io:format("~s => ~s~n", [Name, StateName]).
