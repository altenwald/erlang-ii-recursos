-module(marcador).
-author('manuel@altenwald.com').

-export([start_link/0, stop/0, notify/1, anota/2]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

stop() ->
    gen_event:stop(?MODULE).

notify(Event) ->
    gen_event:notify(?MODULE, Event).

anota(Quien, Cuanto) ->
    notify({puntua, Quien, Cuanto}).
