-module(public_logs_sup).
-author('manuel@altenwald.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 10,
                  period => 5 },
    ChildSpec = #{ id => public_logs,
                   start => {public_logs, start_link, []},
                   restart => permanent,
                   shutdown => 500,
                   type => worker,
                   modules => [public_logs, file] },
    {ok, {SupFlags, [ChildSpec]}}.
