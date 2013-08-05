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
    erl_db_compiler:compile(get_path() ++ File),
    rebar_log:log(info, "Created ~p...~n", [File]),
    compile_models(Files, [File|Acc]).

get_path() ->
    {ok, Dir} = file:get_cwd(),
    filename:join([Dir,"models"]) ++ "/".
