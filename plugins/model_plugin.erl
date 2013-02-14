-module(model_plugin).

-compile(export_all).

-define(PATH, "examples/").

model(_,_) ->
    read_models().

-spec read_models() -> list(atom()) | [].
read_models() ->
    rebar_log:log(info, "Reading models...~n", []),
    {ok, Filenames} = file:list_dir("examples"),
    read_models(Filenames, []).

-spec read_models(list(atom())|[], Acc) -> Acc when
      Acc :: list(atom())|[].
read_models([], Acc) ->
    Acc;
read_models([H|T], Acc) ->
    {module, Module} = test:create(?PATH ++ H),
   %%  rebar_log:log(info, "Created ~p...~n", [Module]),
    read_models(T, [Module|Acc]).
