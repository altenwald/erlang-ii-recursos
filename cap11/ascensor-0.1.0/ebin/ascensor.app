{application, ascensor, [
    {description, "Ascensor"},
    {id, "ascensor"},
    {vsn, "0.1.0"},
    {modules, [ascensor_app,
               ascensor_sup,
               ascensor]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {ascensor_app, []}}
]}.
