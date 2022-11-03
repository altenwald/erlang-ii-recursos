-module(tareas_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one,
                  intensity => 10,
                  period => 5 },
    ChildSpec = #{ id => tareas,
                   start => {tarea, start_link, []},
                   restart => transient,
                   shutdown => brutal_kill },
    {ok, {SupFlags, [ChildSpec]}}.
