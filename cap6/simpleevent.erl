-module(simpleevent).
-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

add_handler(EventMgrRef) ->
    gen_event:add_handler(EventMgrRef, ?MODULE, []).

delete_handler(EventMgrRef) ->
    gen_event:delete_handler(EventMgrRef, ?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event(_Event, State) ->
    {ok, State}.

% optional
handle_info(_Info, State) ->
    {ok, State}.

% optional
terminate([], _State) ->
    ok.

% optional
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
