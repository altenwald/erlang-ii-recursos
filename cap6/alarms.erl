-module(alarms).
-author('manuel@altenwald.com').

-export([start_link/1, stop/0, notify/1, defcon/1]).

to_name(1) -> alarms_defcon1;
to_name(2) -> alarms_defcon2.

start_link(Number) ->
    {ok, PID} = gen_event:start_link({local, ?MODULE}),
    ok = gen_event:add_sup_handler(?MODULE, to_name(Number), []),
    {ok, PID}.

stop() ->
    gen_event:stop(?MODULE).

notify(Event) ->
    gen_event:notify(?MODULE, Event).

defcon(Number) ->
    gen_event:sync_notify(?MODULE, {defcon, Number}).
