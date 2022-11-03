-module(logs_fichero).
-author('manuel@altenwald.com').
-vsn(2).

-behaviour(gen_event).

-export([
    add_handler/1,
    delete_handler/0
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    fichero :: pid(),
    nombre :: string()
}).

add_handler(Nombre) ->
    gen_event:add_handler(logs, ?MODULE, [Nombre]).

delete_handler() ->
    gen_event:delete_handler(logs, ?MODULE, []).

init([Nombre]) ->
    {ok, PID} = file:open(Nombre, [append]),
    {ok, #state{fichero = PID, nombre = Nombre}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, #state{fichero=PID}=State) ->
    file:write(PID, io_lib:format("~p~n", [Event])),
    {ok, State}.

handle_info(rotar, #state{fichero=PID, nombre=Nombre}=State) ->
    ok = file:close(PID),
    ok = file:rename(Nombre, Nombre ++ ".old"),
    {ok, NuevoPID} = file:open(Nombre, [append]),
    {ok, State#state{fichero=NuevoPID}}.

terminate([], #state{fichero=PID}) ->
    file:close(PID),
    ok.

code_change(1, {state, PID}, _Extra) ->
    io:format("subiendo de vsn 1 a vsn 2~n", []),
    {ok, Nombre} = file:pid2name(PID),
    {ok, #state{fichero=PID, nombre=Nombre}}.
