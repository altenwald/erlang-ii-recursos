{application, elevator, [
    {description, "Elevator"},
    {id, "elevator"},
    {vsn, "0.2.0"},
    {modules, [elevator_app,
               elevator_sup,
               elevator]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {elevator_app, []}}
]}.
