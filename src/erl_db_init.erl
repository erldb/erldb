-module(erl_db_init).

-export([start/0]).

-define(PATH, "examples/").

start() ->
    Models = read_models(),
    start_backends(Models),
    erl_db_mnesia:info().

read_models() ->
    io:format("Reading models...~n"),
    {ok, Filenames} = file:list_dir("examples"),
    read_models(Filenames, []).

read_models([], Acc) ->
    Acc;
read_models([H|T], Acc) ->
    {module, Module} = test:create(?PATH ++ H),
    io:format("Created ~p...~n", [Module]),
    read_models(T, [Module|Acc]).

start_backends(Models) ->
    io:format("Starting backends...~n"),
    start_backends(Models, []).

start_backends([], Acc) ->
    Acc;
start_backends([H|T], Acc) ->
    Backend = H:backend(),
    case lists:member(Backend, Acc) of
        true ->
            setup_models(Backend, H),
            start_backends(T, Acc);
        false ->
            A = "erl_db_"++atom_to_list(Backend),
            B = list_to_atom(A),
            B:init(),
            setup_models(Backend, H),
            start_backends(T, [Backend|Acc])
    end.

setup_models(mnesia, Model) ->
    io:format("  Setting up ~p...~n", [Model]),
    erl_db_mnesia:init_table(Model).
