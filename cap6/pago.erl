-module(pago).
-author('manuel@altenwald.com').

-behaviour(gen_statem).

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
    callback_mode/0,
    init/1,
    handle_event/4,
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
    gen_statem:start_link(?MODULE, [], []).

stop(Name) ->
    gen_statem:stop(Name).

da_nombre(PID, Nombre) ->
    gen_statem:call(PID, {nombre, Nombre}).

da_forma_pago(PID, FormaPago) ->
    gen_statem:call(PID, {forma_pago, FormaPago}).

da_tarjeta(PID, Tarjeta) ->
    gen_statem:call(PID, {tarjeta, Tarjeta}).

da_cuenta(PID, Cuenta) ->
    gen_statem:call(PID, {cuenta, Cuenta}).

obtiene_info(PID) ->
    gen_statem:call(PID, info).

callback_mode() ->
    handle_event_function.

init([]) ->
    {ok, credenciales, #state{}}.

handle_event({call, From}, info, StateName, StateData) ->
    {keep_state_and_data, [{reply, From, {StateName, StateData}}]};

handle_event({call, From}, {nombre, Nombre}, credenciales, State) ->
    {next_state, forma_pago, State#state{nombre=Nombre}, [{reply, From, ok}]};
handle_event({call, From}, _Event, credenciales, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "necesitamos nombre!"}}]};

handle_event({call, From}, {forma_pago, tarjeta}, forma_pago, State) ->
    {next_state, pago_tarjeta, State#state{forma_pago=tarjeta},
     [{reply, From, ok}]};
handle_event({call, From}, {forma_pago, domiciliar}, forma_pago, State) ->
    {next_state, pago_cuenta, State#state{forma_pago=domiciliar},
     [{reply, From, ok}]};
handle_event({call, From}, _Event, forma_pago, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "forma de pago: tarjeta o domiciliar"}}]};

handle_event({call, From}, {tarjeta, Tarjeta}, pago_tarjeta, State) ->
    {next_state, pagado, State#state{tarjeta=Tarjeta},
     [{reply, From, {ok,pagado}}, hibernate]};
handle_event({call, From}, _Event, pago_tarjeta, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "necesitamos tarjeta"}}]};

handle_event({call, From}, {cuenta, Cuenta}, pago_cuenta, State) ->
    {next_state, pagado, State#state{cuenta=Cuenta},
     [{reply, From, {ok,pagado}}, hibernate]};
handle_event({call, From}, _Event, pago_cuenta, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "necesitamos cuenta"}}]};

handle_event({call, From}, _Event, pagado, _State) ->
    {keep_state_and_data, [{reply, From, {error, ya_pagado}}, hibernate]}.

terminate(normal, pagado, #state{forma_pago = tarjeta} = State) ->
    io:format("~p Nombre: ~s~nTarjeta: ~s~nPagado.~n",
              [self(), State#state.nombre, State#state.tarjeta]),
    ok;
terminate(normal, pagado, #state{forma_pago = domiciliar} = State) ->
    io:format("~p Nombre: ~s~nCuenta: ~s~nPagado.~n",
              [self(), State#state.nombre, State#state.cuenta]),
    ok;
terminate(normal, _StateName, _StateData) ->
    io:format("~p No pagado.~n", [self()]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
