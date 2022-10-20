-module(filtros).
-author('manuel@altenwald.com').

-export([trace_id/2, peticion/1]).

-include_lib("kernel/include/logger.hrl").

peticion(Traza) ->
    ?LOG_DEBUG("Recibida la traza ~p", [Traza], #{trace_id => Traza}),
    ok.

trace_id(#{meta := #{trace_id := TraceId}} = LogEvent, {trace_id, TraceId}) ->
    LogEvent;

trace_id(#{level := debug}, _Extra) -> stop;
trace_id(#{level := info}, _Extra) -> stop;
trace_id(LogEvent, _Extra) -> LogEvent.
