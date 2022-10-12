-module(simplesup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

child_spec(Module, Type, Args) ->
    #{id => Module,
      start => {Module, start_link, Args},
      restart => permanent,
      shutdown => brute_kill,
      type => Type,
      modules => [Module]}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        % child_spec(my_module, worker, [])
    ],
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 1}, Children}}.
