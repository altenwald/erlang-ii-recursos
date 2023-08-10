-module(traffic_light).
-author('manuel@altenwald.com').

-behaviour(gen_statem).

-export([
    start_link/2,
    stop/0,
    watch_light/0
]).

-export([
    callback_mode/0,
    init/1,
    handle_event/4
]).

-define(AMBER_TIME, 1000). %% 1 second

-record(state, {
    green_time :: pos_integer(),
    red_time :: pos_integer()
}).

start_link(GreenTime, RedTime) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE,
                          [GreenTime, RedTime], []).

stop() ->
    gen_statem:stop(?MODULE).

watch_light() ->
    gen_statem:call(?MODULE, watch_light).

callback_mode() ->
    handle_event_function.

init([GreenTime, RedTime]) ->
    {ok, red, #state{
        green_time = GreenTime,
        red_time = RedTime
    }, [{state_timeout, RedTime, {change, green}}]}.

handle_event(state_timeout, Event, red, State) ->
    io:format("* change to amber~n", []),
    {next_state, amber, State,
     [{state_timeout, ?AMBER_TIME, Event}]};

handle_event(state_timeout, {change, green}, amber, State) ->
    io:format("* change to green~n", []),
    {next_state, green, State,
     [{state_timeout, State#state.green_time, {change, red}}]};

handle_event(state_timeout, {change, red}, amber, State) ->
    io:format("* change to red~n", []),
    {next_state, red, State,
     [{state_timeout, State#state.red_time, {change, green}}]};

handle_event(state_timeout, Event, green, State) ->
    io:format("* change to amber~n", []),
    {next_state, amber, State,
     [{state_timeout, ?AMBER_TIME, Event}]};

handle_event({call, From}, watch_light, StateName, _StateData) ->
    {keep_state_and_data,
     [{reply, From, StateName}]}.
