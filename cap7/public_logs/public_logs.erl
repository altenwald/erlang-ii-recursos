-module(public_logs).
-author('manuel@altenwald.com').

-behaviour(supervisor_bridge).

-export([start_link/0,
         init/1,
         terminate/2,
         write/1,
         read/0]).

-define(FILE, "file.log").

start_link() ->
    supervisor_bridge:start_link({local, ?MODULE}, ?MODULE,
                                 [?FILE]).

write(Bytes) ->
    file:write(whereis(logs), Bytes).

read() ->
    file:position(whereis(logs), bof),
    file:read(logs, 1024).

init([File]) ->
    case file:open(File, [read, write]) of
        {ok, PID} ->
            io:format("pid => ~p~n", [PID]),
            register(logs, PID),
            {ok, PID, PID};
        {error, Error} ->
            {error, Error}
    end.

terminate(_Reason, PID) ->
    case is_process_alive(PID) of
        true -> file:close(PID);
        false -> ok
    end.
