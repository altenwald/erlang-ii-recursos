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
    code_change/3,
    format_status/2
]).

start_link(Servers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Servers, []).

stop() ->
    gen_server:cast(?MODULE, stop).

get_server() ->
    gen_server:call(?MODULE, get).

send_server(Msg) ->
    gen_server:call(?MODULE, {send, Msg}).

add_server(Server) ->
    gen_server:cast(?MODULE, {add, Server}).

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

handle_call(get, _From, Servers) ->
    {Server, BalancedServers} = balance(Servers),
    {reply, Server, BalancedServers};

handle_call({send, Msg}, From, Servers) ->
    {Server, BalancedServers} = balance(Servers),
    spawn(fun() ->
        Reply = gen_server:call(Server, Msg),
        gen_server:reply(From, Reply)
    end),
    {noreply, BalancedServers}.

handle_cast({add, Server}, Servers) ->
    PID = get_pid(Server),
    monitor(process, PID),
    {noreply, queue:in(PID, Servers)};
handle_cast(stop, Servers) ->
    {stop, normal, Servers}.

handle_info({'DOWN', _, _, PID, _}, Servers) ->
    Fun = fun(Server) -> Server =/= PID end,
    CleanedServers = queue:filter(Fun, Servers),
    {noreply, CleanedServers}.

terminate(_Reason, Queue) ->
    Servers = queue:to_list(Queue),
    [ gen_server:cast(Server, stop) || Server <- Servers ],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Type, [_Dict, Queue]) ->
    queue:to_list(Queue).
