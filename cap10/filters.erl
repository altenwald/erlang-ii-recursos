-module(filters).
-author('manuel@altenwald.com').

-export([trace_id/2, request/1]).

-include_lib("kernel/include/logger.hrl").

request(Trace) ->
    ?LOG_DEBUG("Received ~p", [Trace], #{trace_id => Trace}),
    ok.

trace_id(#{meta := #{trace_id := TraceId}} = LogEvent, {trace_id, TraceId}) ->
    LogEvent;

trace_id(#{level := debug}, _Extra) -> stop;
trace_id(#{level := info}, _Extra) -> stop;
trace_id(LogEvent, _Extra) -> LogEvent.
