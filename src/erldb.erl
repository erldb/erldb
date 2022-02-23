%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se> [http://www.novaframework.org]
%%% @doc
%%% Main interface for erldb
%%% @end
%%%-------------------------------------------------------------------
-module(erldb).
-export([
         compile/1,
         compile_model/2,

         find/2,
         save/1,

         create_table/2,
         create_database/1
        ]).

-include("../include/erldb.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%
%% API                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%
compile(App) ->
    erldb_compiler:compile_repo(App).

compile_model(Filepath, Options) ->
    erldb_compiler:compile(Filepath, Options).

create_table(Model, Args) ->
    ok.

create_database(Database) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions   %%
%%%%%%%%%%%%%%%%%%%%%%%%%

get_database_configuration() ->
    {ok, Configuration} = application:get_env(erldb, database_config),
    Configuration.
