-module(logs_file).
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
    file :: pid(),
    name :: string(),
    idx :: pos_integer()
}).

add_handler(Name) ->
    gen_event:add_handler(logs, ?MODULE, {{Name, 0}, undefined}).

delete_handler() ->
    gen_event:delete_handler(logs, ?MODULE, []).

name(Name, Idx) ->
    lists:flatten(io_lib:format("~s_~3..0b.log", [Name, Idx])).

init({{Name, Idx}, _}) ->
    FName = name(Name, Idx),
    {ok, PID} = file:open(FName, [append]),
    State = #state{file = PID,
                   name = Name,
                   idx = Idx},
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, #state{file = PID} = State) ->
    file:write(PID, io_lib:format("~p~n", [Event])),
    {ok, State}.

handle_info(rotate, #state{idx = Idx, name = Name} = State) ->
    {swap_handler, [], State, ?MODULE, {Name, Idx + 1}}.

terminate([], #state{file = PID}) ->
    file:close(PID),
    ok.

code_change(2, {state, PID, Name}, _Extra) ->
    io:format("upgrading from vsn 2 to vsn 3~n", []),
    {ok, Name} = file:pid2name(PID),
    {ok, #state{file = PID, name = Name, idx = 0}};

code_change(1, {state, PID}, _Extra) ->
    io:format("upgrading from vsn 1 to vsn 3~n", []),
    {ok, Name} = file:pid2name(PID),
    {ok, #state{file = PID, name = Name, idx = 0}}.
