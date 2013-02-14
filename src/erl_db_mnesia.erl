-module(erl_db_mnesia).

-export([init/0,
         init_tables/1,
         info/0]).


init() ->
    application:start(mnesia),
    mnesia:create_schema([node()]),
    mnesia:start().

init_tables(Models) ->
    Models.

info() ->
    mnesia:info().
