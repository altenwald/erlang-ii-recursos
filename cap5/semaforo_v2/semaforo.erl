-module(semaforo).
-author('manuel@altenwald.com').
-vsn(2).

-behaviour(gen_fsm).

-export([
    start_link/2,
    stop/0,
    ver_semaforo/0
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

-define(TIEMPO_EN_AMBAR, 1000). %% 1 segundo

-record(state, {
    tiempo_verde :: pos_integer(),
    tiempo_rojo :: pos_integer(),
    siguiente :: verde | rojo
}).

start_link(TiempoVerde, TiempoRojo) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE,
                       [TiempoVerde, TiempoRojo], []).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

ver_semaforo() ->
    gen_fsm:sync_send_all_state_event(?MODULE, ver_semaforo).

init([TiempoVerde, TiempoRojo]) ->
    gen_fsm:send_event_after(TiempoRojo, timeout),
    {ok, rojo, #state{
        tiempo_verde = TiempoVerde,
        tiempo_rojo = TiempoRojo
    }}.

rojo(timeout, State) ->
    gen_fsm:send_event_after(?TIEMPO_EN_AMBAR, timeout),
    {next_state, ambar, State#state{siguiente=verde}}.

ambar(timeout, #state{siguiente=verde}=State) ->
    gen_fsm:send_event_after(State#state.tiempo_verde, timeout),
    {next_state, verde, State};
ambar(timeout, #state{siguiente=rojo}=State) ->
    gen_fsm:send_event_after(State#state.tiempo_rojo, timeout),
    {next_state, rojo, State}.

verde(timeout, State) ->
    gen_fsm:send_event_after(?TIEMPO_EN_AMBAR, timeout),
    {next_state, ambar, State#state{siguiente=rojo}}.

handle_sync_event(ver_semaforo, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, rojo, #state{tiempo_rojo=TiempoRojo}=State, _Extra) ->
    gen_fsm:send_event_after(TiempoRojo, timeout),
    {ok, State};
code_change(_OldVsn, verde, #state{tiempo_rojo=TiempoVerde}=State, _Extra) ->
    gen_fsm:send_event_after(TiempoVerde, timeout),
    {ok, State};
code_change(_OldVsn, ambar, State, _Extra) ->
    gen_fsm:send_event_after(?TIEMPO_EN_AMBAR, timeout),
    {ok, State}.
