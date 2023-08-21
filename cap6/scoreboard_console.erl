-module(scoreboard_console).
-author('manuel@altenwald.com').

-behaviour(gen_event).

-export([
    add_handler/3,
    delete_handler/1,
    results/1,
    history/1
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

-type team() :: string().
-type score() :: pos_integer().
-type goals() :: {erlang:timestamp(), team(), score()}.

-record(state, {
    team1 :: team(),
    team2 :: team(),
    score1 = 0 :: pos_integer(),
    score2 = 0 :: pos_integer(),
    history = [] :: [goals()]
}).

add_handler(Id, Team1, Team2) ->
    gen_event:add_handler(scoreboard, {?MODULE, Id}, [Team1, Team2]).

delete_handler(Id) ->
    gen_event:delete_handler(scoreboard, {?MODULE, Id}, []).

results(Id) ->
    gen_event:call(scoreboard, {?MODULE, Id}, results).

history(Id) ->
    gen_event:call(scoreboard, {?MODULE, Id}, history).

init([Team1, Team2]) ->
    {ok, #state{team1 = Team1, team2 = Team2}}.

handle_call(history, #state{history = History} = State) ->
    Reply = lists:map(fun({Time, Who, Goals}) ->
        {_, {H, M, S}} = calendar:now_to_local_time(Time),
        {{H, M, S}, Who, Goals}
    end, lists:reverse(History)),
    {ok, Reply, State};

handle_call(results, #state{team1 = T1,
                            team2 = T2,
                            score1 = S1,
                            score2 = S2} = State) ->
    {ok, [{E1, P1}, {E2, P2}], State}.

handle_event({goal, T1, N}, #state{team1 = T1,
                                   score1 = S1,
                                   history = History} = State) ->
    io:format("Goal! ~s (~p goals)~n", [T1, N + S1]),
    {ok, State#state{score1 = S1 + N,
                     history = [{os:timestamp(), T1, N} | History]}};

handle_event({goal, T2, N}, #state{team2 = T2,
                                   score2 = S2,
                                   history = History} = State) ->
    io:format("Goal! ~s (~p goals)~n", [T2, N + S2]),
    {ok, State#state{score2 = S2 + N,
                     history = [{os:timestamp(), T2, N} | History]}};

handle_event({goal, _, _}, State) ->
    {ok, State}.

terminate([], State) ->
    io:format("Match summary:~n", []),
    lists:foreach(fun({Time, Who, Goals}) ->
        {_, {H, M, S}} = calendar:now_to_local_time(Time),
        io:format("~2..0b:~2..0b:~2..0b ~s ~b goals~n", [H, M, S, Who, Goals])
    end, lists:reverse(State#state.history)),
    io:format("Final result:~n~20s - ~s~n~20b - ~b~n",
              [State#state.team1, State#state.team2,
               State#state.score1, State#state.score2]),
    #{State#state.team1 => State#state.score1,
      State#state.team2 => State#state.score2}.
