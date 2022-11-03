-module(fibonacci).
-author('manuel@altenwald.com').
-vsn(2).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    get_value/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    format_status/1
]).

-record(state, {
    n1 = 1 :: pos_integer,
    n2 = 1 :: pos_integer
}).

-define(TIMEOUT, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

get_value() ->
    gen_server:call(?MODULE, get_value).

init([]) ->
    {ok, #state{}, ?TIMEOUT}.

handle_call(get_value, _From, #state{n1 = N1, n2 = N2} = State) ->
    N3 = N1 + N2,
    {reply, N1, State#state{n1 = N2, n2 = N3}, ?TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{n1 = N1, n2 = N2} = State) ->
    io:format("N = ~p~n", [N1]),
    N3 = N1 + N2,
    {noreply, State#state{n1 = N2, n2 = N3}, ?TIMEOUT}.

code_change(_OldVsn, State, _Extra) ->
    erlang:send_after(?TIMEOUT, self(), timeout),
    {ok, State}.

format_status(#{state := State} = Status) ->
    maps:put(state, #{
        num1 => State#state.n1,
        num2 => State#state.n2
    }, Status).
