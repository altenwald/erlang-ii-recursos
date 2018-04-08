-module(pago).
-author('manuel@altenwald.com').

-behaviour(gen_fsm).

-export([
    start_link/0,
    stop/1,
    da_nombre/2,
    da_forma_pago/2,
    da_tarjeta/2,
    da_cuenta/2,
    obtiene_info/1
]).

-export([
    init/1,
    credenciales/3,
    forma_pago/3,
    pago_tarjeta/3,
    pago_cuenta/3,
    pagado/3,
    handle_sync_event/4,
    handle_event/3,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-type forma_pago() :: tarjeta | domiciliar.

-record(state, {
	nombre :: string(),
	forma_pago :: forma_pago(),
	tarjeta :: string(),
	cuenta :: string()
}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

stop(Name) ->
    gen_fsm:send_all_state_event(Name, stop).

da_nombre(PID, Nombre) ->
    gen_fsm:sync_send_event(PID, {nombre, Nombre}).

da_forma_pago(PID, FormaPago) ->
    gen_fsm:sync_send_event(PID, {forma_pago, FormaPago}).

da_tarjeta(PID, Tarjeta) ->
    gen_fsm:sync_send_event(PID, {tarjeta, Tarjeta}).

da_cuenta(PID, Cuenta) ->
    gen_fsm:sync_send_event(PID, {cuenta, Cuenta}).

obtiene_info(PID) ->
    gen_fsm:sync_send_all_state_event(PID, info).

init([]) ->
    {ok, credenciales, #state{}}.

credenciales({nombre, Nombre}, _From, State) ->
	{reply, ok, forma_pago, State#state{nombre=Nombre}};
credenciales(_Msg, _From, State) ->
	{reply, {error, "necesitamos nombre!"}, credenciales, State}.

forma_pago({forma_pago, tarjeta}, _From, State) ->
	{reply, ok, pago_tarjeta, State#state{forma_pago=tarjeta}};
forma_pago({forma_pago, domiciliar}, _From, State) ->
	{reply, ok, pago_cuenta, State#state{forma_pago=domiciliar}};
forma_pago(_Msg, _From, State) ->
	{reply, {error, "forma de pago: tarjeta o domiciliar"}, forma_pago, State}.

pago_tarjeta({tarjeta, Tarjeta}, _From, State) ->
	{reply, {ok, pagado}, pagado, State#state{tarjeta=Tarjeta}, hibernate};
pago_tarjeta(_Msg, _From, State) ->
	{reply, {error, "necesitamos tarjeta"}, pago_tarjeta, State}.

pago_cuenta({cuenta, Cuenta}, _From, State) ->
	{reply, {ok, pagado}, pagado, State#state{cuenta=Cuenta}, hibernate};
pago_cuenta(_Msg, _From, State) ->
	{reply, {error, "necesitamos cuenta"}, pago_cuenta, State}.

pagado(_Event, _From, State) ->
    {reply, {error, ya_pagado}, State, hibernate}.

handle_sync_event(info, _From, StateName, StateData) ->
    {reply, {StateName, StateData}, StateName, StateData}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, pagado, #state{forma_pago=tarjeta}=State) ->
    io:format("~p Nombre: ~s~nTarjeta: ~s~nPagado.~n",
              [self(), State#state.nombre, State#state.tarjeta]),
    ok;
terminate(_Reason, pagado, #state{forma_pago=domiciliar}=State) ->
    io:format("~p Nombre: ~s~nCuenta: ~s~nPagado.~n",
              [self(), State#state.nombre, State#state.cuenta]),
    ok;
terminate(_Reason, _StateName, _StateData) ->
    io:format("~p No pagado.~n", [self()]),
    ok.

code_change(_OldVsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.
