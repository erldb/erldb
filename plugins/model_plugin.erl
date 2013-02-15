-module(model_plugin).

-export([model/2]).

-define(PATH, "examples/").

model(_,_) ->
    {ok, Filenames} = file:list_dir("examples"),
    code:add_path("ebin"),
    rebar_log:log(info, "Files: ~p~n", [Filenames]),
    compile_models(Filenames, []).

compile_models([], Acc) ->
    rebar_log:log(info, "Done...~n", []);
compile_models([File|Files], Acc) ->
    {ok, BinStr} = file:read_file(?PATH ++ File),
    Str = erlang:binary_to_list(BinStr),
    {ok, Tokens, _Len} = erl_db_lex:string(Str),
    {ok, ST} = erl_db_parser:parse(Tokens),
    {ok, Module} = erl_db_compiler:compile(ST),
    rebar_log:log(info, "Created ~p...~n", [Module]),
    file:copy(Module, "ebin/" ++ Module),
    file:delete(Module),
    compile_models(Files, [Module|Acc]).

