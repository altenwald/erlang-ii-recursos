-module(scoreboard_file).
-author('manuel@altenwald.com').

-behaviour(gen_event).

-export([
    add_handler/3,
    delete_handler/1
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

-type team() :: string().

-record(state, {
    file :: pid(),
    team1 :: team(),
    team2 :: team(),
    score1 = 0 :: pos_integer(),
    score2 = 0 :: pos_integer()
}).

add_handler(File, Team1, Team2) ->
    gen_event:add_handler(scoreboard, {?MODULE, File}, [File, Team1, Team2]).

delete_handler(Id) ->
    gen_event:delete_handler(scoreboard, {?MODULE, Id}, []).

init([File, Team1, Team2]) ->
    {ok, PID} = file:open(File, [append]),
    {ok, #state{file = PID, team1 = Team1, team2 = Team2}}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_event({goal, T1, N}, #state{team1 = T1,
                                   score1 = S1,
                                   file = PID} = State) ->
    Time = os:timestamp(),
    {_, {H, M, S}} = calendar:now_to_local_time(Time),
    Line = io_lib:format("~2..0b:~2..0b:~2..0b;~s;~p~n", [H, M, S, T1, N + S1]),
    file:write(PID, Line),
    {ok, State#state{score1 = S1 + N}};

handle_event({goal, T2, N}, #state{team2 = T2,
                                   score2 = S2,
                                   file = PID} = State) ->
    Time = os:timestamp(),
    {_, {H, M, S}} = calendar:now_to_local_time(Time),
    Line = io_lib:format("~2..0b:~2..0b:~2..0b;~s;~p~n", [H, M, S, T2, N + S2]),
    file:write(PID, Line),
    {ok, State#state{score2 = S2 + N}};

handle_event({goal, _, _}, State) ->
    {ok, State}.

terminate([], #state{file = PID} = State) ->
    file:write(PID,
               io_lib:format("~n~s;~b~n~s;~b~n",
                             [State#state.team1, State#state.score1,
                              State#state.team2, State#state.score2])),
    file:close(PID),
    ok.
