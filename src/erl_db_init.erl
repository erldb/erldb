-module(erl_db_init).

-export([start/0]).

start() ->
    Models = read_models(),
    start_backends(Models),
    erl_db_mnesia:info().

read_models() ->
    io:format("Reading models...~n"),
    {ok, Filenames} = file:list_dir("examples"),
    [{io:format("Reading ~p~n", [X]), X} || X <- Filenames].

start_backends(_Models) ->
    io:format("Starting backends...~n"),
    io:format("Mnesia...~n"),
    erl_db_mnesia:init().

setup_models(_Models) ->
    io:format("  Setting up models...~n").
