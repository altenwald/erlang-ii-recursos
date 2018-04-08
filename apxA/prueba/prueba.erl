%% @author Manuel Rubio
%% @copyright 2018 Altenwald Solutions, S.L.
%% @doc Este módulo es una prueba. La documentación con esta
%%      etiqueta puede expandirse varias líneas.
%% @end
-module(prueba).

-export([hola_mundo/1,
         hola/1]).

-spec hola_mundo(Nombre :: string()) -> ok.
%% @doc Imprime `Hola Mundo!' seguido de un nombre.
%% @deprecated Usa mejor `hola/1'
hola_mundo(Nombre) ->
    io:format("Hola Mundo! y ~s~n", [Nombre]).

-spec hola(Nombre :: string()) -> ok.
%% @doc Imprime un saludo a una persona.
hola(Nombre) ->
    io:format("Hola ~s~n", [Nombre]).
