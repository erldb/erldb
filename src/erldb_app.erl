-module(erldb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Return = erldb_sup:start_link(),
    Path =
        case application:get_env(model_path) of
            {ok, P} ->
                P;
            undefined ->
                "./models"
        end,
    {ok, Models} = file:list_dir_all(Path),
    bootstrap_models(Path, Models),
    Return.


stop(_State) ->
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

bootstrap_models(_Path, []) ->
    ok;
bootstrap_models(Path, [H|T]) ->
    %% We always compile the models we load.
    {ok, Model, _Filename} = erldb_compiler:compile(filename:join(Path, H)),

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

    TableOptions = proplists:get_value(worker_options, BackendValues),
    Worker = poolboy:checkout(Backend),
    {ok, _Res} = gen_server:call(Worker, {init_table, Model, [{module_attr, Attr}, {worker_options, TableOptions}]}),
    poolboy:checkin(Backend, Worker),
    bootstrap_models(Path, T).
