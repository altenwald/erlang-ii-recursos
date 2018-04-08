-module(semaforo).
-author('manuel@altenwald.com').
-vsn(1).

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
    terminate/2,
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
    gen_fsm:send_all_state(?MODULE, stop).

ver_semaforo() ->
    gen_fsm:sync_send_all_state(?MODUEL, ver_semaforo).

init([TiempoVerde, TiempoRojo]) ->
    {ok, rojo, #state{
        tiempo_verde = TiempoVerde,
        tiempo_rojo = TiempoRojo
    }, TiempoRojo}.

rojo(timeout, State) ->
    {next_state, ambar, State#state{siguiente=verde}, ?TIEMPO_EN_AMBAR}.

ambar(timeout, #state{siguiente=verde}=State) ->
    {next_state, verde, State, State#state.tiempo_verde};
ambar(timeout, #state{siguiente=rojo}=State) ->
    {next_state, rojo, State, State#state.tiempo_rojo}.

verde(timeout, State) ->
    {next_state, ambar, State#state{siguiente=rojo}, ?TIEMPO_EN_AMBAR}.

handle_sync_event(ver_semaforo, rojo, _From, StateData) ->
    {reply, rojo, rojo, StateData, State#state.tiempo_rojo};
handle_sync_event(ver_semaforo, verde, _From, StateData) ->
    {reply, verde, verde, StateData, State#state.tiempo_verde};
handle_sync_event(ver_semaforo, ambar, _From, StateData) ->
    {reply, ambar, ambar, StateData, ?TIEMPO_EN_AMBAR}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, State}.
