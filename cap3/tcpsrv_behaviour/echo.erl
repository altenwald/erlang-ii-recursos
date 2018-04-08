-module(echo).
-behaviour(tcpsrv).

-export([start/1, handle_request/2]).

start(Port) ->
    tcpsrv:start(Port, ?MODULE).

handle_request(Socket, Msg) ->
    io:format("Recibido ~p: ~p~n", [self(), Msg]),
    timer:sleep(5000), %% 5 segundos de espera
    tcpsrv:send(Socket, io_lib:format("Eco: ~s", [Msg])),
    ok.
