-module(recurso_sup).
-author('manuel@altenwald.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_all,
                  intensity => 10,
                  period => 5 },
    ChildrenSpec = [
      #{ id => diccionario,
         start => {diccionario, start_link, []} },
      #{ id => echo_server,
         start => {echo_server, start_link, [self()]} }
    ],
    {ok, {SupFlags, ChildrenSpec}}.
