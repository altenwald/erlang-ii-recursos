-module(tcpsrv).
-export([start/2, send/2]).

start(Port, Module) ->
    spawn(fun() -> srv_init(Port, Module) end).

srv_init(Port, Module) ->
    Opts = [{reuseaddr, true}, {active, false}],
    {ok, Socket} = gen_tcp:listen(Port, Opts),
    srv_loop(Socket, Module).

srv_loop(Socket, Module) ->
    {ok, SockCli} = gen_tcp:accept(Socket),
    Pid = spawn(fun() -> worker_loop(SockCli, Module) end),
    gen_tcp:controlling_process(SockCli, Pid),
    inet:setopts(SockCli, [{active, true}]),
    srv_loop(Socket, Module).

worker_loop(Socket, Module) ->
    receive
        {tcp, Socket, Msg} ->
            Module:handle_request(Socket, Msg),
            worker_loop(Socket, Module);
        {tcp_closed, Socket} ->
            io:format("Finalizado.~n");
        Any ->
            io:format("Mensaje no reconocido: ~p~n", [Any])
    end.

send(Socket, Msg) ->
    gen_tcp:send(Socket, Msg).
