-module(simplestate_state_functions).
-behaviour(gen_statem).

-export([callback_mode/0,
         init/1,
         code_change/4,
         terminate/3]).

%% definir funciones de estado. Ejemplo:
-export([turned_on/3,
         turned_off/3]).

callback_mode() -> state_functions.

-record(state_data, {}).

init([]) ->
    {ok, turned_off, #state_data{}}.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

turned_on(_EventType, _EventContent, Data) ->
    {next_state, turned_off, Data}.

turned_off(_EventType, _EventContent, Data) ->
    {next_state, turned_on, Data}.
