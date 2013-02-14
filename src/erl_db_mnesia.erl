-module(erl_db_mnesia).

-export([init/0,
         init_table/1,
         info/0]).


init() ->
    io:format("Starting Mnesia...~n"),
    application:start(mnesia),
    mnesia:create_schema([node()]).

init_table(Model) ->
    mnesia:create_table(Model, []).

info() ->
    mnesia:info().
