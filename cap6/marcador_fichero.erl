-module(marcador_fichero).
-author('manuel@altenwald.com').

-behaviour(gen_event).

-export([
    add_handler/3,
    delete_handler/1
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

-type equipo() :: string().

-record(state, {
    fichero :: pid(),
    equipo1 :: equipo(),
    equipo2 :: equipo(),
    puntuacion1 = 0 :: pos_integer(),
    puntuacion2 = 0 :: pos_integer()
}).

add_handler(File, Equipo1, Equipo2) ->
    gen_event:add_handler(marcador, {?MODULE, File}, [File, Equipo1, Equipo2]).

delete_handler(Id) ->
    gen_event:delete_handler(marcador, {?MODULE, Id}, []).

init([File, Equipo1, Equipo2]) ->
    {ok, PID} = file:open(File, [append]),
    {ok, #state{fichero = PID, equipo1 = Equipo1, equipo2 = Equipo2}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event({puntua, E1, N}, #state{equipo1 = E1,
                                     puntuacion1 = P1,
                                     fichero=PID}=State) ->
    Hora = os:timestamp(),
    {_,{H,M,S}} = calendar:now_to_local_time(Hora),
    Line = io_lib:format("~2..0b:~2..0b:~2..0b;~s;~p~n", [H,M,S,E1,N+P1]),
    file:write(PID, Line),
    {ok, State#state{puntuacion1=P1+N}};

handle_event({puntua, E2, N}, #state{equipo2 = E2,
                                     puntuacion2 = P2,
                                     fichero=PID}=State) ->
    Hora = os:timestamp(),
    {_,{H,M,S}} = calendar:now_to_local_time(Hora),
    Line = io_lib:format("~2..0b:~2..0b:~2..0b;~s;~p~n", [H,M,S,E2,N+P2]),
    file:write(PID, Line),
    {ok, State#state{puntuacion2=P2+N}};

handle_event({puntua, _, _}, State) ->
    {ok, State}.

terminate([], #state{fichero = PID} = State) ->
    file:write(PID,
               io_lib:format("~n~s;~b~n~s;~b~n",
                             [State#state.equipo1, State#state.puntuacion1,
                              State#state.equipo2, State#state.puntuacion2])),
    file:close(PID),
    ok.
