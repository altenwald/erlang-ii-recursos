-module(dictionary).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/1,
    get_value/1,
    add_value/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

get_value(Key) ->
    poolboy:transaction(dictionary, fun(Server) ->
        gen_server:call(Server, {get, Key})
    end).

add_value(Key, Value) ->
    poolboy:transaction(dictionary, fun(Server) ->
        gen_server:cast(Server, {add, Key, Value})
    end).

init([]) ->
    {ok, undefined}.

handle_call({get, Key}, _From, State) ->
    case ets:lookup(dictionary, Key) of
        [{Key, Value}] -> {reply, Value, State};
        [] -> {reply, undefined, State}
    end.

handle_cast({add, Key, Value}, State) ->
    ets:insert(dictionary, {Key, Value}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
