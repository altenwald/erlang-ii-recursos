-module(elevator).
-author('manuel@altenwald.com').
-vsn(1).

-behaviour(gen_statem).

-export([
    start_link/0,
    stop/0,
    button_up/0,
    button_down/0
]).

-export([
    callback_mode/0,
    init/1,
    ground_floor/3,
    first_floor/3,
    second_floor/3,
    terminate/3,
    code_change/4
]).

-record(state, {}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

button_up() ->
    gen_statem:cast(?MODULE, up).

button_down() ->
    gen_statem:cast(?MODULE, down).

callback_mode() ->
    state_functions.

init([]) ->
    io:format("starting on the ground floor~n", []),
    {ok, ground_floor, #state{}}.

ground_floor(cast, up, StateData) ->
    io:format("going up to the first floor~n", []),
    {next_state, first_floor, StateData};
ground_floor(cast, down, StateData) ->
    io:format("beeeep! cannot go down~n", []),
    {next_state, ground_floor, StateData}.

first_floor(cast, up, StateData) ->
    io:format("going up to the second floor~n", []),
    {next_state, second_floor, StateData};
first_floor(cast, down, StateData) ->
    io:format("going down to the first floor~n", []),
    {next_state, ground_floor, StateData}.

second_floor(cast, up, StateData) ->
    io:format("beeeep! cannot go up~n", []),
    {next_state, second_floor, StateData};
second_floor(cast, down, StateData) ->
    io:format("going down to the first floor~n", []),
    {next_state, first_floor, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
