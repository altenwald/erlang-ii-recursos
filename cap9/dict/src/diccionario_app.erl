-module(diccionario_app).
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start(_StartType, _StartArgs) ->
    ets:new(diccionario, [named_table, set, public]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 1,
                  period => 5 },
    Name = diccionario_pool,
    PoolArgs = [
        {name, {local, diccionario}},
        {worker_module, diccionario},
        {size, 2},
        {max_overflow, 5},
        {strategy, fifo}
    ],
    WorkerArgs = [],
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    {ok, {SupFlags, [ChildSpec]}}.
