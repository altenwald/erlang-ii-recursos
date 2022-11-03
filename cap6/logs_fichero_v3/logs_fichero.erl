-module(logs_fichero).
-author('manuel@altenwald.com').
-vsn(3).

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
    nombre :: string(),
    idx :: pos_integer()
}).

add_handler(Nombre) ->
    gen_event:add_handler(logs, ?MODULE, {{Nombre, 0}, undefined}).

delete_handler() ->
    gen_event:delete_handler(logs, ?MODULE, []).

nombre(Nombre, Idx) ->
    lists:flatten(io_lib:format("~s_~3..0b.log", [Nombre, Idx])).

init({{Nombre, Idx}, _}) ->
    FName = nombre(Nombre, Idx),
    {ok, PID} = file:open(FName, [append]),
    State = #state{fichero = PID,
                   nombre = Nombre,
                   idx = Idx},
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, #state{fichero = PID} = State) ->
    file:write(PID, io_lib:format("~p~n", [Event])),
    {ok, State}.

handle_info(rotar, #state{idx = Idx, nombre = Nombre} = State) ->
    {swap_handler, [], State, ?MODULE, {Nombre, Idx + 1}}.

terminate([], #state{fichero = PID}) ->
    file:close(PID),
    ok.

code_change(2, {state, PID, Nombre}, _Extra) ->
    io:format("subiendo de vsn 2 a vsn 3~n", []),
    {ok, Nombre} = file:pid2name(PID),
    {ok, #state{fichero = PID, nombre = Nombre, idx = 0}};

code_change(1, {state, PID}, _Extra) ->
    io:format("subiendo de vsn 1 a vsn 3~n", []),
    {ok, Nombre} = file:pid2name(PID),
    {ok, #state{fichero = PID, nombre = Nombre, idx = 0}}.
