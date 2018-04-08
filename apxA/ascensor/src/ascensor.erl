%% @doc Código simulando un ascensor con una máquina de estados.
%% @author Manuel Rubio <manuel@altenwald.com> [http://altenwald.org]
%% @copyright 2018 Manuel Rubio
%% @version 2
-module(ascensor).
-author('manuel@altenwald.com').
-vsn(2).

-behaviour(gen_statem).

-export([
    start_link/0,
    stop/0,
    boton_num/1,
    boton_arriba/0,
    boton_abajo/0
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
%% @returns si todo va bien retornará el PID del proceso.
%% @doc Esta función es estándar para la mayoría de procesos en OTP.
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
%% @doc detiene el proceso.
stop() ->
    gen_statem:stop(?MODULE).

-spec boton_num(Num :: pos_integer()) -> ok.
%% @param Num número de planta a la que mover el ascensor.
%% @returns siempre retorna `ok'.
%% @since 2
%% @doc Esta función indica al ascensor la planta a la que
%%      ir o donde pararse.
%% @end
boton_num(Num) ->
    gen_statem:cast(?MODULE, {pressed, Num}).

-spec boton_arriba() -> ok.
%% @returns always returns `ok'.
%% @deprecated mejor usar {@link boton_num/1}.
boton_arriba() ->
    gen_statem:cast(?MODULE, {pressed, up}).

-spec boton_abajo() -> ok.
%% @returns always returns `ok'.
%% @deprecated mejor usar boton_num/1.
boton_abajo() ->
    gen_statem:cast(?MODULE, {pressed, down}).

-spec callback_mode() -> atom().
%% @hidden
callback_mode() ->
    handle_event_function.

-spec time_to_floor() -> pos_integer().
%% @private
%% @doc obtiene el tiempo para cambio de planta.
time_to_floor() ->
    application:get_env(ascensor, time_to_floor, ?TIME_TO_FLOOR).

-spec init([]) -> {ok, atom(), #state{}, [atom()]}.
%% @private
%% @doc función de `gen_statem' para iniciar la máquina de estados.
init([]) ->
    io:format("* iniciando ascensor, planta 0~n", []),
    {ok, stopped, #state{}, [hibernate]}.

-spec add_num(pos_integer(), #state{}) -> #state{}.
%% @private
%% @doc agrega un número a la lista de números presionados.
add_num(Num, #state{pressed = Pressed} = StateData) ->
    NewPressed = sets:add_element(Num, Pressed),
    StateData#state{pressed = NewPressed}.

-spec del_num(pos_integer(), #state{}) -> #state{}.
%% @private
%% @doc elimina un número de la lista de números presionados.
del_num(Num, #state{pressed = Pressed} = StateData) ->
    NewPressed = sets:del_element(Num, Pressed),
    StateData#state{pressed = NewPressed}.

-spec current(#state{}, pos_integer()) -> #state{}.
%% @private
%% @doc especifica la planta actual cambiando el valor de `current_floor'.
current(StateData, Num) ->
    StateData#state{current_floor = Num}.

%% @hidden
%% @doc manejador de eventos de `gen_statem'.
handle_event(cast, {pressed, up}, _Name, #state{current_floor = Current}) ->
    {keep_state_and_data, [{next_event, cast, {pressed, Current + 1}}]};
handle_event(cast, {pressed, down}, _Name, #state{current_floor = Current}) ->
    {keep_state_and_data, [{next_event, cast, {pressed, Current - 1}}]};
handle_event(cast, {pressed, Num}, _Name, _Data)
        when Num =< ?BOTTOM_BORDER_FLOOR orelse
             Num >= ?TOP_BORDER_FLOOR ->
    io:format("x recibida planta incorrecta: ~p~n", [Num]),
    keep_state_and_data;
handle_event(cast, {pressed, Num}, stopped, StateData) ->
    case StateData#state.current_floor of
        Current when Current < Num ->
            NewState = add_num(Num, StateData),
            Next = NewState#state.current_floor + 1,
            Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
            io:format("* cerrando puertas y subiendo~n", []),
            {next_state, going_up, NewState, Actions};
        Current when Current > Num ->
            NewState = add_num(Num, StateData),
            Next = NewState#state.current_floor - 1,
            Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
            io:format("* cerrando puertas y bajando~n", []),
            {next_state, going_down, NewState, Actions};
        Current when Current =:= Num ->
            io:format("> planta actual, puertas abiertas~n", []),
            keep_state_and_data
    end;
handle_event(cast, {pressed, Num}, _StateName, StateData) ->
    NewState = add_num(Num, StateData),
    Nums = lists:sort(sets:to_list(NewState#state.pressed)),
    io:format("+ agregada planta ~p para parar (~p)~n", [Num, Nums]),
    {keep_state, NewState};

handle_event(state_timeout, {stop, Current}, StateName, StateData) ->
    case sets:is_element(Current, StateData#state.pressed) of
        true ->
            io:format("> parando en planta ~p, abriendo puertas~n", [Current]),
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
            io:format("< pasando por planta ~p~n", [Current]),
            Next = case StateName of
                going_up -> Current + 1;
                going_down -> Current - 1
            end,
            Actions = [{state_timeout, time_to_floor(), {stop, Next}}],
            {keep_state, current(StateData, Current), Actions}
    end.

%% @hidden
%% @doc función de terminación del proceso de `gen_statem'.
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% @hidden
%% @doc función de cambio de código en caliente de `gen_statem'.
code_change(1, planta_baja, _OldData, _Extra) ->
    {ok, stopped, #state{current_floor = 0}};
code_change(1, primera_planta, _OldData, _Extra) ->
    {ok, stopped, #state{current_floor = 1}};
code_change(1, segunda_planta, _OldData, _Extra) ->
    {ok, stopped, #state{current_floor = 2}};
code_change({down, 1}, _, StateData, _Extra) ->
    NewState = case StateData#state.current_floor of
        0 -> planta_baja;
        1 -> primera_planta;
        _ -> segunda_planta
    end,
    {ok, NewState, {state}}.
