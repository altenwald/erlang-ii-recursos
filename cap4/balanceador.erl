-module(balanceador).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    get_server/0,
    send_server/1,
    add_server/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    format_status/1
]).

start_link(Servers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Servers, []).

stop() ->
    gen_server:stop(?MODULE).

get_server() ->
    gen_server:call(?MODULE, get_server).

send_server(Msg) ->
    gen_server:call(?MODULE, {send_server, Msg}).

add_server(Server) ->
    gen_server:cast(?MODULE, {add_server, Server}).

get_pid(Name) when is_atom(Name) -> whereis(Name);
get_pid(PID) when is_pid(PID) -> PID.

init(Servers) ->
    PIDs = lists:map(fun(Server) ->
        monitor(process, Server),
        get_pid(Server)
    end, Servers),
    {ok, queue:from_list(PIDs)}.

balance(Servers) ->
    {{value, Server}, Servers1} = queue:out(Servers),
    {Server, queue:in(Server, Servers1)}.

handle_call(get_server, _From, Servers) ->
    {Server, BalancedServers} = balance(Servers),
    {reply, Server, BalancedServers};

handle_call({send_server, Msg}, From, Servers) ->
    {Server, BalancedServers} = balance(Servers),
    spawn(fun() ->
        Reply = gen_server:call(Server, Msg),
        gen_server:reply(From, Reply)
    end),
    {noreply, BalancedServers}.

handle_cast({add_server, Server}, Servers) ->
    PID = get_pid(Server),
    monitor(process, PID),
    {noreply, queue:in(PID, Servers)}.

handle_info({'DOWN', _, _, PID, _}, Servers) ->
    Fun = fun(Server) -> Server =/= PID end,
    CleanedServers = queue:filter(Fun, Servers),
    {noreply, CleanedServers}.

terminate(_Reason, Queue) ->
    queue:filter(fun(Server) ->
        ok =:= gen_server:stop(Server, shutdown, infinity)
    end, Queue),
    ok.

format_status(#{state := Queue} = Status) ->
    maps:put(state, queue:to_list(Queue), Status).
