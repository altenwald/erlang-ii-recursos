-module(tcpsrv_errors).
-export([start/1]).

-spec start(inet:port_number()) -> pid().
start(Port) ->
    spawn(fun() -> srv_init(Port) end).

-spec srv_init(inet:port_number()) -> pos_integer().
srv_init(Port) ->
    Opts = [{reuseaddr, true}, {active, false}],
    {ok, Socket} = gen_tcp:listen(Port, Opts),
    srv_loop(Socket).

-spec srv_loop(integer()) -> no_return().
srv_loop(Socket) ->
    {ok, SockCli} = gen_tcp:accept(Socket),
    Pid = spawn(fun() -> worker_loop(SockCli) end),
    gen_tcp:controlling_process(SockCli, Pid),
    inet:setopts(SockCli, [{active, true}]),
    srv_loop(Socket).

-spec worker_loop(inet:socket()) -> no_return().
worker_loop(Socket) ->
    receive
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, io_lib:format("Eco: ~s", [Msg])),
            worker_loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Finalizado.~n");
        Any ->
            io:format("Mensaje no reconocido: ~p~n", [Any])
    end.
