-module(simpleappsup).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

child_spec(Module, Type, Args) ->
    #{id => Module,
      start => {Module, start_link, Args},
      restart => permanent,
      shutdown => brute_kill,
      type => Type,
      modules => [Module]}.

init([]) ->
    Children = [
        % child_spec(my_module, worker, [])
    ],
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 1}, Children}}.
