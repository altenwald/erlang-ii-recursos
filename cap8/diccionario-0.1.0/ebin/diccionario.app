{application, diccionario, [
    {description, "Diccionario"},
    {id, "diccionario"},
    {vsn, "0.1.0"},
    {modules, [diccionario_app,
               diccionario_sup,
               diccionario]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {diccionario_app, []}}
]}.
