-module(dictionary).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/0,
    stop/1,
    get_value/2,
    add_value/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(TIMEOUT, 5000).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Server) ->
    gen_server:stop(Server).

get_value(Server, Key) ->
    gen_server:call(Server, {get_value, Key}).

add_value(Server, Key, Value) ->
    gen_server:cast(Server, {add_value, Key, Value}).

init([]) ->
    {ok, #{}, ?TIMEOUT}.

handle_call({get_value, Key}, _From, Map) ->
    {reply, maps:get(Key, Map, undefined), Map, ?TIMEOUT}.

handle_cast({add_value, Key, Value}, Map) ->
    {noreply, maps:put(Key, Value, Map), ?TIMEOUT}.

handle_info(timeout, State) ->
    {noreply, State, hibernate}.
