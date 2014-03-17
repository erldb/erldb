-module(erldb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Path} = application:get_env(erldb, model_path),
    {ok, Models} = file:list_dir_all(Path),
    Return = erldb_sup:start_link(),
    erldb_init:ensure_tables(Models),
    Return.


stop(_State) ->
    ok.
