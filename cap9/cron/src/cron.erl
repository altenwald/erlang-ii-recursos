-module(cron).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/0,
    start/0,
    stop/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type hour() :: 0..23.
-type minute() :: 0..59.

-type cron_hourly() :: {hourly, minute(), mfa()}.
-type cron_daily() :: {daily, hour(), minute(), mfa()}.
-type cron_contant() :: {constant, mfa()}.

-type cron_entry() :: cron_hourly() |
                      cron_daily() |
                      cron_contant().

-type cron_table() :: [cron_entry()].

-record(state, {
    table :: cron_table(),
    timer :: timer:time()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    application:start(cron).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    CronTab = application:get_env(cron, table, []),
    {ok, Timer} = timer:send_interval(1000, tick),
    {ok, #state{table = CronTab, timer = Timer}}.

handle_call(_Msg, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(tick, #state{table = Table} = State) ->
    check_table(Table, time()),
    {noreply, State}.

terminate(_Reason, #state{timer = Timer}) ->
    timer:cancel(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_table([], _Time) ->
    ok;
check_table([{hourly, Minute, MFA}|Table],
            {_, Minute, 0} = Time) ->
    execute(MFA),
    check_table(Table, Time);
check_table([{daily, Hour, Minute, MFA}|Table],
            {Hour, Minute, 0} = Time) ->
    execute(MFA),
    check_table(Table, Time);
check_table([{constant, MFA}|Table], {_, _, 0} = Time) ->
    execute(MFA),
    check_table(Table, Time);
check_table([_|Table], Time) ->
    check_table(Table, Time).

execute({Module, Function, Args}) ->
    spawn(fun() -> apply(Module, Function, Args) end).
