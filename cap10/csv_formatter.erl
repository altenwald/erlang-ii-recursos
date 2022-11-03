-module(csv_formatter).
-author('manuel@altenwald.com').

-export([format/2]).

format(#{msg := {string, String}} = LogEvent, Config) ->
    format(LogEvent#{msg => String}, Config);

format(#{msg := {Fmt, Args}} = LogEvent, Config) ->
    Msg = io_lib:format(Fmt, Args),
    format(LogEvent#{msg => Msg}, Config);

format(#{msg := {report, #{} = Map}} = LogEvent, Config) ->
    Proplist = maps:to_list(Map),
    format(LogEvent#{msg => {report, Proplist}}, Config);

format(#{msg := {report, Proplist}} = LogEvent, Config) ->
    Values = lists:map(fun({Key, Value}) ->
      io_lib:format("~p=~p", [Key, Value])
    end, "", Proplist),
    String = lists:join($,, Values),
    format(LogEvent#{msg => String}, Config);

format(#{msg := Msg, level := Level, meta := Meta}, _C) ->
    {Date, Time} = get_date_and_time(Meta),
    io_lib:format("~s;~s;~s;~9999p~n",
                  [Date, Time, Level, Msg]).

get_date_and_time(#{time := Time}) ->
    Opts = [{unit, microsecond}],
    TL = calendar:system_time_to_rfc3339(Time, Opts),
    {lists:sublist(TL, 10), lists:sublist(TL, 12, 8)};

get_date_and_time(#{} = Meta) ->
    get_date_and_time(Meta#{time => logger:timestamp()}).
