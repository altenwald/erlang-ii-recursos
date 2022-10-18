-module(diccionario_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

child(Id) ->
    SupId = list_to_atom("dict_" ++ atom_to_list(Id)),
    #{ id => SupId,
       start => {diccionario, start_link, []},
       restart => permanent,
       shutdown => brutal_kill,
       type => worker,
       modules => [diccionario]}.

init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 1,
                  period => 5 },
    Dic = application:get_env(diccionario, diccionarios, []),
    Children = [ child(Id) || Id <- Dic ],
    {ok, {SupFlags, Children}}.
