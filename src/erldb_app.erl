-module(erldb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Return = erldb_sup:start_link(),
    Models =
        case application:get_env(model_path) of
            {ok, Path} ->
                {ok, ModelsList} = file:list_dir_all(Path),
                ModelsList;
            undefined ->
                []
        end,
    erldb_init:ensure_tables(Models),
    Return.


stop(_State) ->
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

bootstrap_models([]) ->
    ok;
bootstrap_models([H|T]) ->
    %% We always compile the models we load.
    {ok, Model, Binary) = erldb_compiler:compile(H),
    code:load_binary(Model, H, Binary),
    Attr = Model:module_info(attributes),

    %% We should be able to handle default backends
    Backend =
        case proplists:get_value(backend, Attr) of
            undefined ->
                {ok, B} = application:get_env(erldb, default_backend),
                B;
            [{B, _}] ->
                B
        end,

    %% Take out the db_pools information
    {ok, Env} = application:get_env(erldb, db_pools),
    BackendValues = proplists:get_value(Backend, Env),

    TableOptions = proplists:get_value(default_table_options, BackendValues),
    Worker = poolboy:checkout(Backend),

    {ok, Model} = gen_server:call(Worker, {init_table, Model, [{module_attr, Attr} | TableOptions]}),
    poolboy:checkin(Backend, Worker),
    ensure_tables(T).
