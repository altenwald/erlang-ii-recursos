-module(tcpsrv).
-export([start/2, send/2]).

% proc_lib functions
-export([srv_init/2, srv_loop/2, worker_init/2, worker_loop/2]).

-callback handle_request(Socket :: gen_tcp:socket(), Msg :: term()) -> ok.

start(Port, Module) ->
    proc_lib:start(?MODULE, srv_init, [Port, Module]).

srv_init(Port, Module) ->
    Opts = [{reuseaddr, true}, {active, false}],
    {ok, Socket} = gen_tcp:listen(Port, Opts),
    proc_lib:init_ack({ok, self()}),
    srv_loop(Socket, Module).

srv_loop(Socket, Module) ->
    {ok, SockCli} = gen_tcp:accept(Socket),
    {ok, Pid} = proc_lib:start(?MODULE, worker_init, [SockCli, Module]),
    gen_tcp:controlling_process(SockCli, Pid),
    inet:setopts(SockCli, [{active, true}]),
    ?MODULE:srv_loop(Socket, Module).

worker_init(Socket, Module) ->
    proc_lib:init_ack({ok, self()}),
    worker_loop(Socket, Module).

worker_loop(Socket, Module) ->
    receive
        {tcp, Socket, Msg} ->
            Module:handle_request(Socket, Msg),
            ?MODULE:worker_loop(Socket, Module);
        {tcp_closed, Socket} ->
            io:format("END.~n");
        Any ->
            io:format("Unknown message: ~p~n", [Any])
    end.

send(Socket, Msg) ->
    gen_tcp:send(Socket, Msg).
