-module(erl_db_compiler).

-include("../include/erl_db_types.hrl").

-export([compile/1, compile/2]).

-record(state, {
          current_type,
          current_value,
          imported_models,
          backend,
          fields,
          funtions
         }).

parse([#'MODEL'{imports = Imports, name = Name, backend = Backend, fields = Fields, functions = Functions}|_], State) ->
    ImportState = parse(Imports, State#state{imported_models = dict:new()}),
    NameState = parse(Name, ImportState),
    BackendState = parse(Backend, NameState),
    FieldsState = parse(Fields, BackendState),
    parse(Functions, FieldsState);

parse([], State) -> State;
parse([#'BACKEND'{name = {'identifier', Backendname, Line, _TokenLen}, arguments = Arguments}|Tl], State) ->
    %% Control that the backend config is defined
    ConfigBackends = erl_db_env:get_env(erl_db, db_pools, []),
    case get_backend(Backendname, ConfigBackends) of
        undefined ->
            erl_db_log:msg(error, "Could not find the specified backend-config. Declared as ~p on line ~p", [Backendname, Line]),
            throw(backend_not_found);
        _Value ->
            ok
    end,
    NewState = parse(Arguments, State#state{current_type = backend}),
    parse(Tl, NewState);
parse([#'IMPORT'{model = {'identifier', Modelname, Line, _TokenLen}}|Tl], State = #state{imported_models = ImportedModels}) ->
    %% We need to check if the model is loaded or not
    case load_imports(Modelname) of
        undefined ->
            erl_db_log:msg(error, "Could not find the model '~p' declared on line ~p", [Modelname, Line]),
            throw(model_not_found);
        Fields ->
            UpdatedImportedModels = dict:store(Modelname, Fields, ImportedModels),
            parse(Tl, State#state{imported_models = UpdatedImportedModels})
    end;
parse([#'FIELD'{name = Name, type = Type, arguments = Args}|Tl], State) ->
    parse(Name, State),
    TypeState = parse(Type, State),
    parse(Args, TypeState),

    %% Validate values of fields
    case TypeState#state.current_type of
        FieldType when FieldType == foreign_key; FieldType == one_to_many ->
            case proplists:get_value(model_field, Args) of
                undefined ->
                    {_, Fieldname, Line, _} = Name,
                    erl_db_log:msg(error, "No 'model.field'-argument given to field '~p' of type ~p. Line ~p", [Fieldname, FieldType, Line]),
                    throw(foreign_key_error);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,

    parse(Tl, State);

%% Type definitions
parse([{'identifier', id, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = id});
parse([{'identifier', string, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = string});
parse([{'identifier', binary, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = binary});
parse([{'identifier', integer, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = integer});
parse([{'identifier', float, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = float});
parse([{'identifier', datetime, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = datetime});
parse([{'identifier', timestamp, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = timestamp});
parse([{'identifier', boolean, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = boolean});
parse([{'identifier', primary_key, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_type = primary_key});
parse([{'identifier', foreign_key, _Line, _Len}|Tl], State) ->
    %% We should require a model_field when this type
    parse(Tl, State#state{current_type = foreign_key});
parse([{'identifier', one_to_many, _Line, _Len}|Tl], State) ->
    %% We should require a model_field when this type
    parse(Tl, State#state{current_type = one_to_many});
parse([{'identifier', Name, _Line, _Len}|Tl], State) ->
    parse(Tl, State#state{current_value = Name});

%% Parsing of argument-lists
%% TODO: Build another record for argument-lists
parse([{model_field, {Model, Field}}|Tl], State = #state{current_type = CT, imported_models = IM}) when CT == one_to_many;
                                                                                                        CT == foreign_key ->
    {'identifier', Modelname, Line, _Len} = Model,
    {'identifier', Fieldname, _Line2, _Len2} = Field,

    case dict:find(Modelname, IM) of
        {ok, Fields} ->
            case get_field(Fieldname, Fields) of
                undefined ->
                    erl_db_log:msg(error, "Field '~p' not found in target model '~p'. Check your spelling and ensure that the given field exists. Line ~p.", [Fieldname, Modelname, Line]),
                    throw(field_not_found);
                _Field ->
                    ok
            end;
        _ ->
            erl_db_log:msg(error, "Modelname '~p' is used but not imported, line ~p.", [Modelname, Line]),
            throw(model_not_imported)
    end,
    parse(Tl, State);
parse([{model_field, {Model, _Field}}|_Tl], _State) ->
    erl_db_log:msg(error, "Error on line ~p. Declaration of type model_field invalid.", [get_line_number(Model)]),
    throw(faulty_type);

parse([nil|Tl], State) ->
    parse(Tl, State);

parse([{_Key, _Value}|Tl], State) ->
    parse(Tl, State);
parse([_Value|Tl], State) ->
    parse(Tl, State);
parse(Element, State) when not is_list(Element) ->
    parse([Element], State).

compile(Filename) ->
    compile(Filename, []).

compile(Filename, Options) ->
    case file:read_file(Filename) of
        {error, Reason} ->
            erl_db_log:msg(error, "Error while reading ~p. Reason: ~p", [Filename, Reason]),
            throw(file_read_error);
        {ok, BinStr} ->
            Str = erlang:binary_to_list(BinStr),
            {ok, Tokens, _Len} = erl_db_lex:string(Str),
            {ok, Model} = erl_db_parser:parse(Tokens),

            #'MODEL'{imports = _Imports, name = Name, backend = #'BACKEND'{name = Backend, arguments = _BackendArgs}, fields = Fields, functions = _Functions} = Model,

            %% First we need to check that the syntax tree is fine
            parse(Model, #state{}),

            ModuleAST = module_ast(Name),

            BackendGetFunctionAST =
                function_ast("backend", [], none, [erl_syntax:atom(extract(Backend))]),
            BackendGetFunction1AST =
                function_ast("backend", [erl_syntax:underscore()], none, [erl_syntax:atom(extract(Backend))]),

            FieldsfunctionAST =
                function_ast("fields", [], none, [proplist_from_fields(Fields)]),

            Fieldsfunction2AST =
                function_ast("fields", [erl_syntax:underscore()], none, [proplist_from_fields(Fields)]),

            SaveFunctionAST = save_function_ast(Name),

            DeleteFunctionAST = delete_function_ast(Name),

            GettersAST = [ getter_ast(Name, Field) || Field <- Fields ],
            SettersAST = [ setter_ast(Name, Field) || Field <- Fields ],

            NewFunctionAST = new_function_ast(Name, Fields),

            FunctionRecordAST = create_record_from_fields(Name, Fields),

            CompileAST = erl_syntax:attribute(erl_syntax:atom(compile), [erl_syntax:atom("export_all")]),

            Forms = [ erl_syntax:revert(AST) || AST <- [ModuleAST,
                                                        CompileAST,
                                                        FunctionRecordAST,
                                                        BackendGetFunctionAST,
                                                        BackendGetFunction1AST,
                                                        NewFunctionAST,
                                                        FieldsfunctionAST,
                                                        Fieldsfunction2AST,
                                                        SaveFunctionAST,
                                                        DeleteFunctionAST
                                                       ] ++ GettersAST ++ SettersAST ],

            OutDir = proplists:get_value(out_dir, Options, "."),
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
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        INTERNAL FUNCTIONS                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract(Val) when is_atom(Val) ->
    Val;
extract({Key, Val}) ->
    {extract(Key), extract(Val)};
extract({identifier, Name, _, _}) ->
    Name.

atom_to_var(Name) when is_atom(Name) ->
    [FirstChar|Tl] = erlang:atom_to_list(Name),
    case FirstChar of
        LowerCase when LowerCase > 96 andalso LowerCase < 123 ->
            [LowerCase-32|Tl];
        _ ->
            [FirstChar|Tl]
    end.

load_imports(Modelname) ->
        case code:is_loaded(Modelname) of
            {file, _Loaded} ->
                Modelname:fields();
            false ->
                %% We need to see if we can compile this model
                %% First we need to find it (How?)
                undefined
        end.

get_line_number({'identifier', _Ident, Line, _Len}) ->
    Line;
get_line_number({'int_constant', _Value, Line}) ->
    Line;
get_line_number({_, Line}) ->
    Line.

get_field(Key, MaybeProplist) ->
    case proplists:get_value(Key, MaybeProplist) of
        undefined ->
            get_field1(Key, MaybeProplist);
        Val ->
            Val
    end.

get_field1(_, []) ->
    undefined;
get_field1(Key, [Tuple = {Key, _Type, _Args}|_Tl]) ->
    Tuple;
get_field1(Key, [_|Tl]) ->
    get_field1(Key, Tl).

get_backend(_, []) ->
    undefined;
get_backend(Key, [{Key, _Module, Args, _}|_Tl]) ->
    Args;
get_backend(Key, [_Element|Tl]) ->
    get_backend(Key, Tl).





%% SYNTAX STUFF

convert_type({identifier, Type, _, _}) when is_atom(Type) ->
    erl_syntax:atom(Type);
convert_type({int_constant, Int, _}) when is_integer(Int) ->
    erl_syntax:integer(Int);
convert_type(Type) when is_atom(Type) ->
    erl_syntax:atom(Type);
convert_type(Type) when is_integer(Type) ->
    erl_syntax:integer(Type);
convert_type({Model, Field}) ->
    erl_syntax:tuple([erl_syntax:atom("model_field"),
                      erl_syntax:tuple([
                                        erl_syntax:atom(extract(Model)),
                                        erl_syntax:atom(extract(Field))
                                        ])
                      ]);
convert_type(Type) ->
    erl_db_log:msg(error, "Unknown type: ~p. Line ~p", [Type, get_line_number(Type)]),
    {error, unknown_type}.

convert_argument([]) -> [];
convert_argument([{Key, Val}|Tl]) ->
    [erl_syntax:tuple([convert_type(Key), convert_type(Val)])|convert_argument(Tl)];
convert_argument([Key|Tl]) ->
    [convert_type(Key)|convert_argument(Tl)];
convert_argument(P) ->
    {error, unknown_type, P}.

proplist_from_fields(Fields) when is_list(Fields) ->
    FieldList = [ proplist_from_fields(X) || X <- Fields ],
    erl_syntax:list(FieldList);

proplist_from_fields(#'FIELD'{name = {identifier, Name, _, _}, type = {identifier, Type, _, _}, arguments = Args}) ->
    erl_syntax:tuple([erl_syntax:atom(extract(Name)), erl_syntax:atom(extract(Type)), erl_syntax:list(convert_argument(Args))]).

create_record_from_fields({identifier, Modulename, _, _}, Fields) ->
    RecordFields =
        lists:map(
          fun(#'FIELD'{name = {identifier, Name, _, _}}) ->
                  erl_syntax:record_field(erl_syntax:atom(Name))
          end,
          Fields),

    erl_syntax:attribute(
      erl_syntax:atom(record),
      [erl_syntax:atom(Modulename),
       erl_syntax:tuple(RecordFields)]
     ).


module_ast({identifier, Modulename, _TokenLine, _TokenLength}) ->
    erl_syntax:attribute(erl_syntax:atom(module),
                         [erl_syntax:atom(Modulename)]).


delete_function_ast({identifier, Modulename, _, _}) ->
    function_ast(delete, [erl_syntax:variable(atom_to_var(Modulename))], none,
                 [
                  erl_syntax:application(
                    erl_syntax:atom(erl_db),
                    erl_syntax:atom(delete),
                    [
                     erl_syntax:variable(atom_to_var(Modulename))
                    ]
                   )]).

save_function_ast({identifier, Modulename, _, _}) ->
    function_ast(save, [erl_syntax:variable(atom_to_var(Modulename))], none,
                 [
                  erl_syntax:application(
                    erl_syntax:atom(erl_db),
                    erl_syntax:atom(save),
                    [
                     erl_syntax:variable(atom_to_var(Modulename))
                    ]
                   )]).

function_ast({identifier, Name, _, _}, Arguments, Guards, Body) ->
    function_ast(Name, Arguments, Guards, Body);

function_ast(Functionname, Arguments, Guards, Body) ->
    erl_syntax:function(erl_syntax:atom(Functionname),
                        [erl_syntax:clause(
                           Arguments, Guards,
                           Body)]).

getter_ast(Modulename, #'FIELD'{name = FunctionName, type = Mapper, arguments = Args}) when Mapper == foreign_key; Mapper == one_to_many ->
    ListArgs = [ extract(X) || X <- Args ],
    case proplists:get_value(target, ListArgs) of
        {error, _Reason} ->
            erl_db_log:msg(error, "Could not compile model ~p. Foreign key ~p does not have a target. ~p.",
                           [Modulename, FunctionName, Modulename]),
            throw(could_not_find_foreign_target);
        Target ->
            [TargetModel, TargetField] = re:split(Target, "\\.", [{return, list}]),
            function_ast(FunctionName, [erl_syntax:variable(atom_to_var(Modulename))], none,
                         [erl_syntax:application(
                           erl_syntax:atom(erl_db),
                           erl_syntax:atom(find),
                           [erl_syntax:atom(TargetModel),
                            erl_syntax:list([
                                             erl_syntax:tuple([
                                                               erl_syntax:atom(TargetField),
                                                               erl_syntax:record_access(erl_syntax:variable(atom_to_var(Modulename)),
                                                                                        erl_syntax:atom(Modulename),
                                                                                        erl_syntax:atom(FunctionName))])])
                            ])])
    end;
getter_ast({identifier, Modulename, _, _}, #'FIELD'{name = {identifier, FunctionName, _, _}}) ->
    function_ast(FunctionName, [erl_syntax:variable(atom_to_var(Modulename))], none,
                 [erl_syntax:record_access(erl_syntax:variable(atom_to_var(Modulename)), erl_syntax:atom(Modulename), erl_syntax:atom(FunctionName))]).


setter_ast({identifier, Modulename, _, _}, #'FIELD'{name = {identifier, FunctionName, _, _}}) ->
    function_ast(FunctionName, [
                                erl_syntax:variable(atom_to_var(FunctionName)),
                                erl_syntax:variable(atom_to_var(Modulename))
                               ], none,
                 [erl_syntax:record_expr(
                    erl_syntax:variable(atom_to_var(Modulename)),
                    erl_syntax:atom(Modulename),
                    [erl_syntax:record_field(
                       erl_syntax:atom(FunctionName),
                       erl_syntax:variable(atom_to_var(FunctionName))
                      )])]).

new_function_ast({identifier, ModuleName, _, _}, Fields) ->
    Args =
        lists:map(
          fun(#'FIELD'{name = {identifier, Name, _, _}}) ->
                  erl_syntax:variable(atom_to_var(Name))
          end,
          Fields),

    FieldSetters =
        lists:map(
          fun(#'FIELD'{name = {identifier, Name, _, _}}) ->
                  erl_syntax:record_field(erl_syntax:atom(Name), erl_syntax:variable(atom_to_var(Name)))
          end,
          Fields),

    function_ast("new", Args, none, [erl_syntax:record_expr(none, erl_syntax:atom(ModuleName), FieldSetters)]).
