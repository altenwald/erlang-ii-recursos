-module(error_line).
-author(manuel@altenwald.com).

-behaviour(gen_event).

-export([
    start/0
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2
]).

-record(state, {}).

start() ->
    gen_event:swap_handler(error_logger, 
                           {error_logger_tty_h, []},
                           {?MODULE, []}).

init(_) ->
    {ok, #state{}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(Event, State) ->
    io:format("~s ~s~n", [get_date(), get_event(Event)]),
    {ok, State}.

get_date() ->
    {Y,M,D} = date(),
    {H,I,S} = time(),
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
                  [Y,M,D,H,I,S]).

get_event({error_report, _, {_, _, Msg}}) ->
    io_lib:format("[ERROR REPORT] ~p", [Msg]);

get_event({error, _, {_, Format, Args}}) ->
    io_lib:format("[ERROR] " ++ Format, Args);

get_event({info_report, _, {_, _, Msg}}) ->
    io_lib:format("[INFO REPORT] ~p", [Msg]);

get_event({info_msg, _, {_, Format, Args}}) ->
    io_lib:format("[INFO] " ++ Format, Args).
