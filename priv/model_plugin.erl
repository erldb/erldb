-module(model_plugin).

-export([model/2]).

model(_,_) ->
    {ok, Filenames} = find_models(),
    code:add_path("ebin"),
    rebar_log:log(info, "Files: ~p~n", [Filenames]),
    compile_models(Filenames, []).

find_models() ->
    case file:list_dir(get_path()) of
	{ok, FileNames} ->
	    {ok, FileNames};
	_ ->
	    {ok, []}
    end.

compile_models([], Acc) ->
    rebar_log:log(info, "Done...~n", []);
compile_models([File|Files], Acc) ->
    case file:read_file(get_path() ++ File) of
        {ok, BinStr} ->
            Str = erlang:binary_to_list(BinStr),
            {ok, Tokens, _Len} = erl_db_lex:string(Str),
            {ok, ST} = erl_db_parser:parse(Tokens),
            {ok, Module} = erl_db_compiler:compile(ST),
            rebar_log:log(info, "Created ~p...~n", [Module]),
            file:copy(Module, "ebin/" ++ Module),
            file:delete(Module),
            compile_models(Files, [Module|Acc]);
        Error ->
            io:format("File: ~p~nPath: ~p~nError: ~p~n", [File, get_path(), Error]),
            compile_models(Files, Acc)
    end.

get_path() ->
    {ok, Dir} = file:get_cwd(),
    filename:join([Dir,"models"]) ++ "/".
