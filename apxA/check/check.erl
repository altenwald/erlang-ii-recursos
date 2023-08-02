%% @author Manuel Rubio
%% @copyright 2018 Altenwald Solutions, S.L.
%% @doc Este módulo es una prueba. La documentación con esta
%%      etiqueta puede expandirse varias líneas.
%% @end
-module(check).

-export([hello_world/1,
         hello/1]).

-spec hello_world(Name :: string()) -> ok.
%% @doc Prints `Hello World!' followed by a name.
%% @deprecated Use better `hello/1'
hello_world(Name) ->
    io:format("Hello World! ~s~n", [Name]).

-spec hello(Name :: string()) -> ok.
%% @doc Prints a greet for a person.
hello(Name) ->
    io:format("Hello ~s~n", [Name]).
