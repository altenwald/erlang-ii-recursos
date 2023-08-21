-module(logs_file).
-author('manuel@altenwald.com').
-vsn(1).

-behaviour(gen_event).

-export([
    add_handler/1,
    delete_handler/0
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

-record(state, {
    file :: pid()
}).

add_handler(Name) ->
    gen_event:add_handler(logs, ?MODULE, [Name]).

delete_handler() ->
    gen_event:delete_handler(logs, ?MODULE, []).

init([Name]) ->
    {ok, PID} = file:open(Name, [append]),
    {ok, #state{file = PID}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, #state{file = PID} = State) ->
    file:write(PID, io_lib:format("~p~n", [Event])),
    {ok, State}.

terminate([], #state{file = PID}) ->
    file:close(PID),
    ok.
