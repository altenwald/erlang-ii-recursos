{application, cron, [
    {description, "Cron"},
    {id, "cron"},
    {vsn, "0.1.0"},
    {modules, [cron_app,
               cron]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, [
        {table, [
            {daily, 9, 0, {io, format, ["morning!~n"]}},
            {hourly, 0, {io, format, ["ding! dong!~n"]}},
            {constant, {io, format, ["minute!~n"]}}
        ]}
    ]},
    {mod, {cron_app, []}}
]}.
