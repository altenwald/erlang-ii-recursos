-module(diccionario).
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
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/2
]).

-define(TIMEOUT, 5000).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Server) ->
    gen_server:cast(Server, stop).

get_value(Server, Key) ->
    gen_server:call(Server, {get, Key}).

add_value(Server, Key, Value) ->
    gen_server:cast(Server, {add, Key, Value}).

init([]) ->
    {ok, dict:new(), ?TIMEOUT}.

handle_call({get, Key}, _From, Dict) ->
    {reply, dict:find(Key, Dict), Dict, ?TIMEOUT}.

handle_cast({add, Key, Value}, Dict) ->
    {noreply, dict:store(Key, Value, Dict)};
handle_cast(stop, Dict) ->
    {stop, normal, Dict}.

handle_info(timeout, State) ->
    {noreply, State, hibernate}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Type, [_Dict, Dict]) ->
    dict:to_list(Dict).
