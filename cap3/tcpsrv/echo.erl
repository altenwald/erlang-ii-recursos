-module(echo).
-export([start/1, handle_request/2]).

start(Port) ->
    tcpsrv:start(Port, ?MODULE).

handle_request(Socket, Msg) ->
    io:format("Received ~p: ~p~n", [self(), Msg]),
    timer:sleep(5_000), %% wait 5 seconds
    tcpsrv:send(Socket, io_lib:format("Echo: ~s", [Msg])),
    ok.
