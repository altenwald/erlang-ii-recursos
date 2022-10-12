-module(diccionario_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 1,
                  period => 5 },
    ChildSpec = #{ id => diccionario,
                   start => {diccionario, start_link, []},
                   restart => permanent,
                   shutdown => brutal_kill },
    {ok, {SupFlags, [ChildSpec]}}.
