-module(echo_server).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    echo/2
]).

-record(state, {
    supervisor :: pid()
}).

start_link(Supervisor) when is_pid(Supervisor) ->
    gen_server:start_link(?MODULE, [Supervisor], []).

echo(Supervisor, Msg) ->
    Child = get_child(Supervisor, echo_server),
    gen_server:call(Child, {echo, Msg}).

init([Supervisor]) ->
    {ok, #state{supervisor = Supervisor}}.

handle_call({echo, Msg}, From, State) ->
    Child = get_child(State#state.supervisor, diccionario),
    diccionario:add_value(Child, Msg, From),
    {reply, Msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

get_child(Supervisor, Id) ->
    Children = supervisor:which_children(Supervisor),
    case lists:keyfind(Id, 1, Children) of
        {Id, PID, _Type, _Module} -> PID;
        false -> error
    end.
