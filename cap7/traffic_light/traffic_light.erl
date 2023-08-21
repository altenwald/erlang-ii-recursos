-module(traffic_light).
-author('manuel@altenwald.com').

-behaviour(gen_statem).

-export([
    start_link/1,
    start_link/2,
    stop/1,
    watch_light/1
]).

-export([
    callback_mode/0,
    init/1,
    handle_event/4
]).

-define(AMBER_TIME, 3_000). %% 3 seconds
-define(RED_TIME, 5_000). %% 3 seconds
-define(GREEN_TIME, 5_000). %% 5 seconds

-record(state, {
    green_time :: pos_integer(),
    red_time :: pos_integer(),
    links = [] :: [atom()],
    name :: atom()
}).

start_link(Name) ->
    gen_statem:start_link({local, Name}, ?MODULE,
                          [Name], []).

start_link(Name, Links) ->
    gen_statem:start_link({local, Name}, ?MODULE,
                          [Name, Links], []).

stop(Name) ->
    gen_statem:stop(Name).

watch_light(Name) ->
    gen_statem:call(Name, watch_light).

callback_mode() ->
    handle_event_function.

init([Name]) ->
    init([Name, []]);

init([Name, Links]) when is_list(Links) ->
    StateData = #state{
        green_time = ?GREEN_TIME,
        red_time = RedTime = ?RED_TIME,
        links = Links,
        name = Name
    },
    case Links of
        [] ->
            print_state(init, StateData),
            print_state(red, StateData),
            InitialState = red,
            Actions = [];
        _ ->
            print_state(init, StateData),
            InitialState = green,
            Actions = [
                {state_timeout, RedTime, {change, red}}
            ]
    end,
    {ok, InitialState, StateData, Actions}.

handle_event(state_timeout, Event, red, State) ->
    print_state(amber, State),
    lists:foreach(fun(Link) ->
        case Event of
            {change, red} ->
                gen_statem:cast(Link, {change, green});
            {change, green} ->
                gen_statem:cast(Link, {change, red})
        end
    end, State#state.links),
    {next_state, amber, State,
     [{state_timeout, ?AMBER_TIME, Event}]};

handle_event(cast, {change, _} = Event, red, State) ->
    print_state(amber, State),
    {next_state, amber, State,
     [{state_timeout, ?AMBER_TIME, Event}]};

handle_event(cast, {change, _}, amber, _State) ->
    {keep_state_and_data, [postpone]};

handle_event(state_timeout, {change, green}, amber, State) ->
    print_state(green, State),
    {next_state, green, State,
     [{state_timeout, State#state.green_time, {change, red}}]};

handle_event(state_timeout, {change, red}, amber, State) ->
    print_state(red, State),
    {next_state, red, State,
     [{state_timeout, State#state.red_time, {change, green}}]};

handle_event(state_timeout, Event, green, State) ->
    print_state(amber, State),
    lists:foreach(fun(Link) ->
        case Event of
            {change, red} ->
                gen_statem:cast(Link, {change, green});
            {change, green} ->
                gen_statem:cast(Link, {change, red})
        end
    end, State#state.links),
    {next_state, amber, State,
     [{state_timeout, ?AMBER_TIME, Event}]};

handle_event(cast, {change, _} = Event, green, State) ->
    print_state(amber, State),
    {next_state, amber, State,
     [{state_timeout, ?AMBER_TIME, Event}]};

handle_event({call, From}, watch_light, StateName, _StateData) ->
    {keep_state_and_data,
     [{reply, From, StateName}]}.

print_state(StateName, #state{name = Name}) ->
    io:format("~s => ~s~n", [Name, StateName]).
