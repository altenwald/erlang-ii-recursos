-module(ascensor).
-author('manuel@altenwald.com').

-behaviour(gen_fsm).

-export([
    start_link/0,
    stop/0,
    boton_arriba/0,
    boton_abajo/0
]).

-export([
    init/1,
    planta_baja/2,
    primera_planta/2,
    segunda_planta/2,
    handle_sync_event/4,
    handle_event/3,
    handle_info/3,
    terminate/3,
    code_change/4
]).

-record(state, {}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

boton_arriba() ->
    gen_fsm:send_event(?MODULE, up).

boton_abajo() ->
    gen_fsm:send_event(?MODULE, down).

init([]) ->
    io:format("iniciamos en el primer piso~n", []),
    {ok, planta_baja, #state{}}.

planta_baja(up, StateData) ->
    io:format("subiendo al primer piso~n", []),
    {next_state, primera_planta, StateData};
planta_baja(down, StateData) ->
    io:format("beeeep! no se puede bajar~n", []),
    {next_state, planta_baja, StateData}.

primera_planta(up, StateData) ->
    io:format("subiendo al segundo piso~n", []),
    {next_state, segunda_planta, StateData};
primera_planta(down, StateData) ->
    io:format("bajando a planta baja~n", []),
    {next_state, planta_baja, StateData}.

segunda_planta(up, StateData) ->
    io:format("beeeep! no se puede subir~n", []),
    {next_state, segunda_planta, StateData};
segunda_planta(down, StateData) ->
    io:format("bajando al primer piso~n", []),
    {next_state, primera_planta, StateData}.

handle_sync_event(_Event, StateName, _From, StateData) ->
    {reply, ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
