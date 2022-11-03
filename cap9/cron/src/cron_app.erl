-module(cron_app).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 1,
                  period => 5 },
    Child = #{ id => cron,
               start => {cron, start_link, []},
               restart => permanent,
               shutdown => brutal_kill,
               type => worker,
               modules => [cron]},
    {ok, {SupFlags, [Child]}}.
