-module(notificaciones).
-behaviour(gen_event).

-export([
    start/0
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

start() ->
    gen_event:swap_handler(alarm_handler, 
                           {alarm_handler, []},
                           {?MODULE, []}).

init(_) ->
    {ok, #state{}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, State) ->
    io:format("~s ~s~n", [get_date(), get_event(Event)]),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate([], _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_date() ->
    {Y,M,D} = date(),
    {H,I,S} = time(),
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
                  [Y,M,D,H,I,S]).

get_event({set_alarm, {_, _}=Msg}) ->
    io_lib:format("[ALARM] ~p", [Msg]);
get_event({clear_alarm, AlarmId}) ->
    io_lib:format("[CLEAR] ~p", [AlarmId]).
