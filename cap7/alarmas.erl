-module(alarmas).
-author('manuel@altenwald.com').

-export([start_link/1, stop/0, notify/1, defcon/1]).

to_name(1) -> alarmas_defcon1;
to_name(2) -> alarmas_defcon2.

start_link(Numero) ->
    {ok, PID} = gen_event:start_link({local, ?MODULE}),
    ok = gen_event:add_sup_handler(?MODULE, to_name(Numero), []),
    {ok, PID}.

stop() ->
    gen_event:stop(?MODULE).

notify(Event) ->
    gen_event:notify(?MODULE, Event).

defcon(Numero) ->
    gen_event:sync_notify(?MODULE, {defcon, Numero}).
