-module(traffic_light_sup).
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
    Links = [traffic_light2, traffic_light3],
    ChildSpec = [
        ChildBase#{ id => traffic_light1,
                    start => {traffic_light, start_link,
                              [traffic_light1, Links]}},
        ChildBase#{ id => traffic_light2,
                    start => {traffic_light, start_link,
                              [traffic_light2]}},
        ChildBase#{ id => traffic_light3,
                    start => {traffic_light, start_link,
                              [traffic_light3]}}
    ],
    {ok, {SupFlags, ChildSpec}}.
