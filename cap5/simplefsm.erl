-module(simplefsm).
-behaviour(gen_fsm).

-export([start_link/0]).

-export([
    init/1,
    init_state/2,
    init_state/3,
    handle_sync_event/4,
    handle_event/3,
    handle_info/3,
    terminate/2,
    code_change/4
]).

-record(state, {}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, init_state, #state{}}.

init_state(_Event, StateData) ->
    {next_state, init_state, StateData}.

init_state(_Event, _From, StateData) ->
    {reply, ok, init_state, StateData}.

handle_sync_event(_Event, StateName, _From, StateData) ->
    {reply, ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, State}.
