-module(simplestate_state_functions).
-behaviour(gen_statem).

-export([callback_mode/0,
         init/1,
         code_change/4,
         terminate/3]).

%% definir funciones de estado. Ejemplo:
-export([encendido/3,
         apagado/3]).

callback_mode() -> state_functions.

-record(state_data, {}).

init([]) ->
    {ok, apagado, #state_data{}}.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

encendido(EventType, EventContent, Data) ->
    {next_state, apagado, Data}.

apagado(EventType, EventContent, Data) ->
    {next_state, encendido, Data}.
