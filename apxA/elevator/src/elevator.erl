%% @doc Simulating an elevator with a state machine.
%% @author Manuel Rubio <manuel@altenwald.com> [http://altenwald.org]
%% @copyright 2018 Manuel Rubio
%% @version 2
-module(elevator).
-author('manuel@altenwald.com').
-vsn(2).

-behaviour(gen_statem).

-export([
    start_link/0,
    stop/0,
    button_num/1,
    button_up/0,
    button_down/0
]).

-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3,
    code_change/4
]).

-define(MAX_FLOORS, 10).
-define(TOP_BORDER_FLOOR, ?MAX_FLOORS + 1).
-define(BOTTOM_BORDER_FLOOR, -1).
-define(TIME_TO_FLOOR, 1000).

-record(state, {
    pressed = sets:new() :: sets:sets(),
    current_floor = 0 :: pos_integer(),
    time_to_floor :: pos_integer()
}).

-spec start_link() -> {ok, pid()} | {error, atom()}.
%% @returns if everything goes well it will return the process ID.
%% @doc This function is standard for the most of the OTP processes.
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
%% @doc stops the process.
stop() ->
    gen_statem:stop(?MODULE).

-spec button_num(Num :: pos_integer()) -> ok.
%% @param Num floor number where the elevator should go.
%% @returns always returns `ok'.
%% @since 2
%% @doc This function indicates to the elevator the floor where
%%      it should go.
%% @end
button_num(Num) ->
    gen_statem:cast(?MODULE, {pressed, Num}).

-spec button_up() -> ok.
%% @returns always returns `ok'.
%% @deprecated better use {@link button_num/1}.
button_up() ->
    gen_statem:cast(?MODULE, {pressed, up}).

-spec button_down() -> ok.
%% @returns always returns `ok'.
%% @deprecated better use {@link button_num/1}.
button_down() ->
    gen_statem:cast(?MODULE, {pressed, down}).

-spec callback_mode() -> atom().
%% @hidden
callback_mode() ->
    handle_event_function.

-spec time_to_floor() -> pos_integer().
%% @private
%% @doc obtain the time remaining for floor staying.
time_to_floor() ->
    application:get_env(elevator, time_to_floor, ?TIME_TO_FLOOR).

-spec init([]) -> {ok, atom(), #state{}, [atom()]}.
%% @private
%% @doc init the `gen_statem' behaviour for the state machine.
init([]) ->
    io:format("* initiating elevator, floor 0~n", []),
    {ok, stopped, #state{}, [hibernate]}.

-spec add_num(pos_integer(), #state{}) -> #state{}.
%% @private
%% @doc adds a number to the list of pressed numbers.
add_num(Num, #state{pressed = Pressed} = StateData) ->
    NewPressed = sets:add_element(Num, Pressed),
    StateData#state{pressed = NewPressed}.

-spec del_num(pos_integer(), #state{}) -> #state{}.
%% @private
%% @doc deletes a number from the list of pressed numbers.
del_num(Num, #state{pressed = Pressed} = StateData) ->
    NewPressed = sets:del_element(Num, Pressed),
    StateData#state{pressed = NewPressed}.

-spec current(#state{}, pos_integer()) -> #state{}.
%% @private
%% @doc specify the current floor changing the value for `current_floor'.
current(StateData, Num) ->
    StateData#state{current_floor = Num}.

%% @hidden
%% @doc event handler for `gen_statem' behaviour.
handle_event(cast, {pressed, up}, _Name, #state{current_floor = Current}) ->
    {keep_state_and_data, [{next_event, cast, {pressed, Current + 1}}]};
handle_event(cast, {pressed, down}, _Name, #state{current_floor = Current}) ->
    {keep_state_and_data, [{next_event, cast, {pressed, Current - 1}}]};
handle_event(cast, {pressed, Num}, _Name, _Data)
        when Num =< ?BOTTOM_BORDER_FLOOR orelse
             Num >= ?TOP_BORDER_FLOOR ->
    io:format("x received an incorrect floor number: ~p~n", [Num]),
    keep_state_and_data;
handle_event(cast, {pressed, Num}, stopped, StateData) ->
    case StateData#state.current_floor of
        Current when Current < Num ->
            NewState = add_num(Num, StateData),
            Next = NewState#state.current_floor + 1,
            Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
            io:format("* closing doors and going up~n", []),
            {next_state, going_up, NewState, Actions};
        Current when Current > Num ->
            NewState = add_num(Num, StateData),
            Next = NewState#state.current_floor - 1,
            Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
            io:format("* closing doors and going down~n", []),
            {next_state, going_down, NewState, Actions};
        Current when Current =:= Num ->
            io:format("> current floor, doors opened~n", []),
            keep_state_and_data
    end;
handle_event(cast, {pressed, Num}, _StateName, StateData) ->
    NewState = add_num(Num, StateData),
    Nums = lists:sort(sets:to_list(NewState#state.pressed)),
    io:format("+ added floor ~p as future stop (~p)~n", [Num, Nums]),
    {keep_state, NewState};

handle_event(state_timeout, {stop, Current}, StateName, StateData) ->
    case sets:is_element(Current, StateData#state.pressed) of
        true ->
            io:format("> stopping in floor ~p, opening doors~n", [Current]),
            NewState = del_num(Current, current(StateData, Current)),
            {Up, Down} = lists:partition(fun(E) -> E > Current end,
                                         sets:to_list(NewState#state.pressed)),
            case {length(Up), length(Down), StateName} of
                {0, 0, _} ->
                    {next_state, stopped, NewState};
                {UpL, _, going_up} when UpL > 0 ->
                    Next = Current + 1,
                    Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
                    {keep_state, NewState, Actions};
                {0, _, going_up} ->
                    Next = Current - 1,
                    Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
                    {next_state, going_down, NewState, Actions};
                {_, DownL, going_down} when DownL > 0 ->
                    Next = Current - 1,
                    Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
                    {keep_state, NewState, Actions};
                {_, 0, going_down} ->
                    Next = Current + 1,
                    Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
                    {next_state, going_up, NewState, Actions}
            end;
        false ->
            io:format("< going through floor ~p~n", [Current]),
            Next = case StateName of
                going_up -> Current + 1;
                going_down -> Current - 1
            end,
            Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
            {keep_state, current(StateData, Current), Actions}
    end.

%% @hidden
%% @doc termination function for the `gen_statem' process.
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% @hidden
%% @doc code change function for hot-swap code.
code_change(1, ground_floor, _OldData, _Extra) ->
    {ok, stopped, #state{current_floor = 0}};
code_change(1, first_floor, _OldData, _Extra) ->
    {ok, stopped, #state{current_floor = 1}};
code_change(1, second_floor, _OldData, _Extra) ->
    {ok, stopped, #state{current_floor = 2}};
code_change({down, 1}, _, StateData, _Extra) ->
    NewState = case StateData#state.current_floor of
        0 -> ground_floor;
        1 -> first_floor;
        _ -> second_floor
    end,
    {ok, NewState, {state}}.
