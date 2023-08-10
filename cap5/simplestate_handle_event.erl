-module(simplestate_handle_event).
-behaviour(gen_statem).

-export([callback_mode/0,
         init/1,
         code_change/4,
         terminate/3,
         handle_event/4]).

callback_mode() -> handle_event_function.

-record(state_data, {}).

init([]) ->
    {ok, turned_off, #state_data{}}.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

handle_event(_EventType, _EventContent, turned_on, Data) ->
    {next_state, turned_off, Data};

handle_event(_EventType, _EventContent, turned_off, Data) ->
    {next_state, turned_on, Data}.
