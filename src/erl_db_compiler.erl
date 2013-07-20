-module(erl_db_compiler).

-include("../include/erl_db_types.hrl").

-export([
         compile/1,
         compile/2
        ]).

-record(state, {
          out_dir,
          backend,
          abs_tree_attr = [],
          abs_tree_flds = []
         }).


-spec compile(string()) -> ok.
compile(Filename) when is_list(Filename) ->
    compile(Filename, []).

compile(Filename, Options) when is_list(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Str = erlang:binary_to_list(Bin),
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Tree} = erl_db_parser:parse(Tokens),
    compile_tree(Tree, Options).

compile_tree(Tree, Options) ->
    OutDir = proplists:get_value(out_dir, Options, "ebin"),
    State = parse(Tree, #state{out_dir = OutDir}),
    Name = proplists:get_value(name, State#state.abs_tree_attr),
    [IncludeDir|_] = proplists:get_value(include_dirs, Options, ["include"]),
    RecordStr = generate_hrl(State),
    file:make_dir(IncludeDir),
    ok = file:write_file(filename:join([IncludeDir, atom_to_list(Name) ++ ".hrl"]), RecordStr),
    erl_db_log:msg(info, "Saved field-declaration file in ~p", [filename:join([IncludeDir, atom_to_list(Name) ++ ".hrl"])]),
    generate_functions(Name, State),
    ok.

parse([], State) -> State;
parse([List], State) when is_list(List) -> parse(List, State);

parse([#'ATTRIBUTE'{key = {atom, _, 'backend'}, value = {atom, Line, Value}, arguments = Args}|Tl], State = #state{abs_tree_attr = AbsTree}) ->
    %% We might want to warn the user if using a backend that isn't defined
    ConfigBackends = erl_db_env:get_env(erl_db, db_pools, []),
    ValidBackend = [ X || {X, _, _, _} <- ConfigBackends, X == Value ],
    case length(ValidBackend) of
        1 ->
            ok;
        _ ->
            erl_db_log:msg(warning, "Could not find the specified backend-config. Declared as ~p on line ~p", [Value, Line])
    end,
    parse(Tl, State#state{backend = {Value, Args}, abs_tree_attr = [{backend, Value}|AbsTree]});

parse([#'ATTRIBUTE'{key = Key, value = Value, arguments = _Args}|Tl], State = #state{abs_tree_attr = AbsTree}) ->
    parse(Tl, State#state{abs_tree_attr = [{unwrap(Key), unwrap(Value)}|AbsTree]});

parse([#'FIELD'{name = Name, type = {atom, Line, foreign_key}, arguments = Args}|Tl], State = #state{abs_tree_flds = AbsTree}) ->
    %% This is a bit tricky since we need to determine if it's a valid target or not
    Target = lists:filter(fun(#'FIELD_REF'{}) -> true; (_) -> false end, Args),
    case Target of
        [] ->
            erl_db_log:msg(error, "No target specified for foreign key ~p, declared on line ~p", [unwrap(Name), Line]),
            throw({error, reference_not_found});
        [#'FIELD_REF'{model = {atom, _, Model}, field = {atom, _, Field}}] ->
            case code:is_loaded(Model) of
                false ->
                    code:load(Model);
                _ ->
                    ok
            end,
            case erlang:function_exported(Model, Field, 1) of
                true ->
                    %% it's okay. Module is compiled, loaded and the key exists
                    ok;
                _ ->
                    %% Warn the user
                    erl_db_log:msg(warning, "Could not find specified field ~p in the model ~p.", [Field, Model]),
                    ok
            end,
            parse(Tl, State#state{abs_tree_flds = [{foreign_key, Model, Field}|AbsTree]})
    end;

parse([#'FIELD'{name = Name, type = Type, arguments = Args}|Tl], State = #state{abs_tree_flds = AbsTree}) ->
    ParsedArgs = [ parse_args(Arg) || Arg <- Args ],
    parse(Tl, State#state{abs_tree_flds = [{unwrap(Name), unwrap(Type), ParsedArgs}|AbsTree]}).

parse_args({Arg1, Arg2}) ->
    {parse_args(Arg1), parse_args(Arg2)};
parse_args({_, _, Val}) ->
    Val.

unwrap({_Type, _, Val}) ->
    Val.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CODE GENERATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_hrl(State) ->
    RecordName = proplists:get_value(name, State#state.abs_tree_attr),
    lists:concat(["-record(", RecordName, ", {", generate_hrl_fields(lists:reverse(State#state.abs_tree_flds)), "})."]).

generate_hrl_fields([]) -> [];
generate_hrl_fields([{Name, Type, Args}|Tl]) ->
    Res =
        case proplists:get_value(default, Args) of
            undefined ->
                case Type of
                    primary_key ->
                        lists:concat([Name, " = id :: any()"]);
                    _ ->
                        lists:concat([Name, " :: ", undefined, " | ", convert_to_erl_type(Type)])
                end;
            DefaultValue ->
                lists:concat([Name, " = \"", DefaultValue, "\" :: ", convert_to_erl_type(Type)])
        end,
    case length(Tl) of
        0 ->
            Res ++ "\n" ++ generate_hrl_fields(Tl);
        _ ->
            Res ++ ",\n" ++ generate_hrl_fields(Tl)
    end.

generate_functions(Modelname, State = #state{out_dir = OutDir}) ->
    AST =
        [
         %% Header
         erl_syntax:attribute(erl_syntax:atom(module),
                              [erl_syntax:atom(Modelname)])] ++
        [
         %% Fields attribute
         erl_syntax:attribute(erl_syntax:atom(fields), [ erl_syntax:list( [ erl_syntax:tuple([ erl_syntax:atom(Fieldname), erl_syntax:atom(Fieldvalue), convert_val_to_syntax(FieldArgs)]) || {Fieldname, Fieldvalue, FieldArgs} <- State#state.abs_tree_flds ] ) ])
        ] ++
        [
         %% Backend attribute
         erl_syntax:attribute(erl_syntax:atom(backend), [ convert_val_to_syntax(State#state.backend) ])
        ] ++
        [
         %% include header file
         erl_syntax:attribute(erl_syntax:atom(include), [ erl_syntax:string(filename:join(["include/", atom_to_list(Modelname) ++ ".hrl"])) ]),

         %% export_all attribute
         erl_syntax:attribute(erl_syntax:atom(compile), [erl_syntax:atom("export_all")]),

         %% Save function
         erl_syntax:function(erl_syntax:atom("save"),
                             [erl_syntax:clause(
                                [erl_syntax:variable("Model")], none, [erl_syntax:application(erl_syntax:atom(erl_db), erl_syntax:atom(save), [ erl_syntax:variable("Model") ])])]),

         %% Delete function
         erl_syntax:function(erl_syntax:atom("delete"),
                             [erl_syntax:clause(
                                [erl_syntax:variable("Model")], none, [erl_syntax:application(erl_syntax:atom(erl_db), erl_syntax:atom(delete), [ erl_syntax:variable("Model") ])])])
        ],
    Forms = [ erl_syntax:revert(Form) || Form <- AST ],

    case compile:forms(Forms) of
        {ok,ModuleName,Binary} ->
            BeamFilename = filename:join([OutDir, atom_to_list(ModuleName) ++ ".beam"]),
            erl_db_log:msg(info, "Compiled model at: ~p", [BeamFilename]),
            file:write_file(BeamFilename, Binary),
            {ok, BeamFilename};
        {ok,ModuleName,Binary,Warnings} ->
            erl_db_log:msg(warning, "Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
            BeamFilename = filename:join([OutDir, atom_to_list(ModuleName) ++ ".beam"]),
            erl_db_log:msg(info, "Compiled model at: ~p", [BeamFilename]),
            file:write_file(BeamFilename, Binary),
            {ok, BeamFilename};
        {error, Reason} ->
            erl_db_log:msg(error, "Error in compilation: ~p", [Reason]);
        {error, Errors, _Warnings} ->
            erl_db_log:msg(error, "Could not compile model: ~p. Exited with errors: ~p~n", [Errors])
    end.



convert_val_to_syntax({atom, _Line, Value}) ->
    convert_val_to_syntax(Value);
convert_val_to_syntax({integer, _Line, Value}) ->
    convert_val_to_syntax(Value);
convert_val_to_syntax({string, _Line, Value}) ->
    convert_val_to_syntax(Value);
convert_val_to_syntax({float, _Line, Value}) ->
    convert_val_to_syntax(Value);
convert_val_to_syntax(List) when is_list(List) ->
    erl_syntax:list([ convert_val_to_syntax(Item) || Item <- List ]);
convert_val_to_syntax(Atom) when is_atom(Atom) ->
    erl_syntax:atom(Atom);
convert_val_to_syntax(Integer) when is_integer(Integer) ->
    erl_syntax:integer(Integer);
convert_val_to_syntax(String) when is_list(String) ->
    erl_syntax:string(String);
convert_val_to_syntax(Float) when is_float(Float) ->
    erl_syntax:float(Float);
convert_val_to_syntax({Key, Val}) ->
    erl_syntax:tuple([convert_val_to_syntax(Key), convert_val_to_syntax(Val)]).




convert_to_erl_type(primary_key) ->
    "string()";
convert_to_erl_type(string) ->
    "string()";
convert_to_erl_type(datetime) ->
    "tuple()";
convert_to_erl_type(integer) ->
    "integer()";
convert_to_erl_type(float) ->
    "float()";
convert_to_erl_type(_) ->
    "any()".
