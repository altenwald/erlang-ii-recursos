-module(logs_file).
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
    file :: pid(),
    name :: string()
}).

add_handler(Name) ->
    gen_event:add_handler(logs, ?MODULE, [Name]).

delete_handler() ->
    gen_event:delete_handler(logs, ?MODULE, []).

init([Name]) ->
    {ok, PID} = file:open(Name, [append]),
    {ok, #state{file = PID, name = Name}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, #state{file = PID} = State) ->
    file:write(PID, io_lib:format("~p~n", [Event])),
    {ok, State}.

handle_info(rotate, #state{file = PID, name = Name} = State) ->
    ok = file:close(PID),
    ok = file:rename(Name, Name ++ ".old"),
    {ok, NewPID} = file:open(Name, [append]),
    {ok, State#state{file = NewPID}}.

terminate([], #state{file = PID}) ->
    file:close(PID),
    ok.

code_change(1, {state, PID}, _Extra) ->
    io:format("upgrading from vsn 1 to vsn 2~n", []),
    {ok, Name} = file:pid2name(PID),
    {ok, #state{file = PID, name = Name}}.
