-module(resources_sup).
-author('manuel@altenwald.com').

-behaviour(supervisor).

-export([start_link/0, init/1, add_child/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one,
                  intensity => 10,
                  period => 5 },
    ChildSpec = #{ id => resource_sup,
                   start => {resource_sup, start_link, []},
                   type => supervisor },
    {ok, {SupFlags, [ChildSpec]}}.
