{application, dictionary, [
    {description, "Dictionary"},
    {id, "dictionary"},
    {vsn, "0.1.0"},
    {modules, [dictionary_app,
               dictionary_sup,
               dictionary]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {dictionary_app, []}}
]}.
