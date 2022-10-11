-module(ascensor).
-author('manuel@altenwald.com').
-vsn(1).

-behaviour(gen_statem).

-export([
    start_link/0,
    stop/0,
    boton_arriba/0,
    boton_abajo/0
]).

-export([
    callback_mode/0,
    init/1,
    planta_baja/3,
    primera_planta/3,
    segunda_planta/3,
    terminate/3,
    code_change/4
]).

-record(state, {}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

boton_arriba() ->
    gen_statem:cast(?MODULE, up).

boton_abajo() ->
    gen_statem:cast(?MODULE, down).

callback_mode() ->
    state_functions.

init([]) ->
    io:format("iniciamos en el primer piso~n", []),
    {ok, planta_baja, #state{}}.

planta_baja(cast, up, StateData) ->
    io:format("subiendo al primer piso~n", []),
    {next_state, primera_planta, StateData};
planta_baja(cast, down, StateData) ->
    io:format("beeeep! no se puede bajar~n", []),
    {next_state, planta_baja, StateData}.

primera_planta(cast, up, StateData) ->
    io:format("subiendo al segundo piso~n", []),
    {next_state, segunda_planta, StateData};
primera_planta(cast, down, StateData) ->
    io:format("bajando a planta baja~n", []),
    {next_state, planta_baja, StateData}.

segunda_planta(cast, up, StateData) ->
    io:format("beeeep! no se puede subir~n", []),
    {next_state, segunda_planta, StateData};
segunda_planta(cast, down, StateData) ->
    io:format("bajando al primer piso~n", []),
    {next_state, primera_planta, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
