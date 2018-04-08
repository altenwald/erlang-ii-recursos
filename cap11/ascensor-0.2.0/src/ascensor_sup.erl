-module(ascensor_sup).
-author('manuel@altenwald.com').

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 1,
                  period => 5 },
    ChildSpec = #{ id => ascensor,
                   start => {ascensor, start_link, []},
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker,
                   modules => [ascensor]},
    {ok, {SupFlags, [ChildSpec]}}.
