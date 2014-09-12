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
