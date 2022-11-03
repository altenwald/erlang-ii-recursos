-module(semaforo_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => rest_for_one,
                  intensity => 10,
                  period => 1 },
    ChildBase = #{ restart => permanent,
                   shutdown => brutal_kill },
    Links = [semaforo2, semaforo3],
    ChildSpec = [
        ChildBase#{ id => semaforo1,
                    start => {semaforo, start_link,
                              [semaforo1, Links]}},
        ChildBase#{ id => semaforo2,
                    start => {semaforo, start_link,
                              [semaforo2]}},
        ChildBase#{ id => semaforo3,
                    start => {semaforo, start_link,
                              [semaforo3]}}
    ],
    {ok, {SupFlags, ChildSpec}}.
