-module(erldb_init).

-export([start/0]).

-define(PATH, "examples/").

-spec start() -> ok.
start() ->
    Models = read_models(),
    _Backends = start_backends(Models),
    erldb_mnesia:info().

-spec read_models() -> list(atom()) | [].
read_models() ->
    io:format("Reading models...~n"),
    {ok, Filenames} = file:list_dir("examples"),
    read_models(Filenames, []).

-spec read_models(list(atom())|[], Acc) -> Acc when
      Acc :: list(atom())|[].
read_models([], Acc) ->
    Acc;
read_models([H|T], Acc) ->
    {module, Module} = test:create(?PATH ++ H),
    io:format("Created ~p...~n", [Module]),
    read_models(T, [Module|Acc]).

-spec start_backends(Models) -> Acc when
      Models :: list(atom())|[],
      Acc :: list(atom())|[].
start_backends(Models) ->
    io:format("Starting backends...~n"),
    start_backends(Models, []).

-spec start_backends(list(), Acc) -> Acc when
      Acc :: list(atom())|[].
start_backends([], Acc) ->
    Acc;
start_backends([H|T], Acc) ->
    Backend = H:backend(),
    case lists:member(Backend, Acc) of
        true ->
            setup_models(Backend, H),
            start_backends(T, Acc);
        false ->
            init_backend(Backend),
            setup_models(Backend, H),
            start_backends(T, [Backend|Acc])
    end.

-spec init_backend(atom()) -> ok.
init_backend(mnesia) ->
    erldb_mnesia:init().

-spec setup_models(atom(), Model) -> ok when
      Model :: atom().
setup_models(mnesia, Model) ->
    io:format("  Setting up ~p...~n", [Model]),
    erldb_mnesia:init_table(Model).
