-module(erldb_init).

-export([ensure_tables/1]).


ensure_tables([]) ->
    io:format("done... ~n", []);
ensure_tables([H|T]) ->
    Model = list_to_atom(filename:basename(H, ".erl")),
    Attr = Model:module_info(attributes),
    [{Backend, _Args}] = proplists:get_value(backend, Attr),
    {ok, Env} = application:get_env(erldb, db_pools),
    BackendValues = proplists:get_value(Backend, Env),
    Worker = proplists:get_value(worker, BackendValues),
    TableOptions = proplists:get_value(default_table_options, BackendValues),
    Worker:init_table(Model, TableOptions),
    ensure_tables(T).
