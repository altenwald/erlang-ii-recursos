-module(fibonacci).
-author('manuel@altenwald.com').
-vsn(1).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    get_value/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {
    n1 = 1 :: pos_integer(),
    n2 = 1 :: pos_integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

get_value() ->
    gen_server:call(?MODULE, get_value).

init([]) ->
    {ok, #state{}}.

handle_call(get, _From, #state{n1 = N1, n2 = N2} = State) ->
    N3 = N1 + N2,
    {reply, N1, State#state{n1 = N2, n2 = N3}}.

handle_cast(_Msg, State) ->
    {noreply, State}.
