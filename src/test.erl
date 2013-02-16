-module(test).
-export([token/1, parse/1, create/1]).

token(File) ->
    {ok, BinStr} = file:read_file(File),
	Str = erlang:binary_to_list(BinStr),
	{ok, Tokens, _Len} = erl_db_lex:string(Str),
    Tokens.

parse(File) ->
	{ok, BinStr} = file:read_file(File),
	Str = erlang:binary_to_list(BinStr),
	{ok, Tokens, _Len} = erl_db_lex:string(Str),
    erl_db_parser:parse(Tokens).

create(File) ->
	{ok, BinStr} = file:read_file(File),
	Str = erlang:binary_to_list(BinStr),
	{ok, Tokens, _Len} = erl_db_lex:string(Str),
    {ok, ST} = erl_db_parser:parse(Tokens),
    erl_db_compiler:compile(ST).
