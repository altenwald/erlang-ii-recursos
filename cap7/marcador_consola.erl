-module(marcador_consola).
-author('manuel@altenwald.com').

-behaviour(gen_event).

-export([
    add_handler/3,
    delete_handler/1,
    resultados/1,
    historico/1
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type equipo() :: string().
-type puntos() :: pos_integer().
-type tantos() :: {erlang:timestamp(), equipo(), puntos()}.

-record(state, {
    equipo1 :: equipo(),
    equipo2 :: equipo(),
    puntuacion1 = 0 :: pos_integer(),
    puntuacion2 = 0 :: pos_integer(),
    historico = [] :: [tantos()]
}).

add_handler(Id, Equipo1, Equipo2) ->
    gen_event:add_handler(marcador, {?MODULE, Id}, [Equipo1, Equipo2]).

delete_handler(Id) ->
    gen_event:delete_handler(marcador, {?MODULE, Id}, []).

resultados(Id) ->
    gen_event:call(marcador, {?MODULE, Id}, resultados).

historico(Id) ->
    gen_event:call(marcador, {?MODULE, Id}, historico).

init([Equipo1, Equipo2]) ->
    {ok, #state{equipo1 = Equipo1, equipo2 = Equipo2}}.

handle_call(historico, #state{historico=Historico}=State) ->
    Reply = lists:map(fun({Hora, Quien, Tantos}) ->
        {_,{H,M,S}} = calendar:now_to_local_time(Hora),
        {{H,M,S},Quien,Tantos}
    end, lists:reverse(Historico)),
    {ok, Reply, State};
handle_call(resultados, #state{equipo1=E1,
                               equipo2=E2,
                               puntuacion1=P1,
                               puntuacion2=P2}=State) ->
    {ok, [{E1,P1},{E2,P2}], State}.

handle_event({puntua, E1, N}, #state{equipo1 = E1,
                                     puntuacion1 = P1,
                                     historico = Historico}=State) ->
    io:format("Gol! de ~s (~p tantos)~n", [E1, N+P1]),
    {ok, State#state{puntuacion1=P1+N,
                     historico=[{os:timestamp(),E1,N}|Historico]}};
handle_event({puntua, E2, N}, #state{equipo2 = E2,
                                     puntuacion2 = P2,
                                     historico = Historico}=State) ->
    io:format("Gol! de ~s (~p tantos)~n", [E2, N+P2]),
    {ok, State#state{puntuacion2=P2+N,
                     historico=[{os:timestamp(),E2,N}|Historico]}};
handle_event({puntua, _, _}, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate([], State) ->
    io:format("Resumen del partido:~n", []),
    lists:foreach(fun({Hora, Quien, Tantos}) ->
        {_,{H,M,S}} = calendar:now_to_local_time(Hora),
        io:format("~2..0b:~2..0b:~2..0b ~s ~b tantos~n", [H,M,S,Quien,Tantos])
    end, lists:reverse(State#state.historico)),
    io:format("Resultado final:~n~20s - ~s~n~20b - ~b~n",
              [State#state.equipo1, State#state.equipo2,
               State#state.puntuacion1, State#state.puntuacion2]),
    [{State#state.equipo1, State#state.puntuacion1,
      State#state.equipo2, State#state.puntuacion2}].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
