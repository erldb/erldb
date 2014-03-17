-module(erldb_init).

-export([ensure_tables/1]).


ensure_tables([]) ->
    ok;
ensure_tables([H|T]) ->
    Model = list_to_atom(filename:basename(H, ".erl")),
    Attr = Model:module_info(attributes),
    [{Backend, _Args}] = proplists:get_value(backend, Attr),
    {ok, Env} = application:get_env(erldb, db_pools),
    BackendValues = proplists:get_value(Backend, Env),
    TableOptions = proplists:get_value(default_table_options, BackendValues),
    Worker = poolboy:checkout(Backend),
    gen_server:call(Worker, {init_table, Model, TableOptions}),
    poolboy:checkin(Backend, Worker),
    ensure_tables(T).
