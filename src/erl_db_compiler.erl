-module(erl_db_compiler).

-include("../include/erl_db_types.hrl").

%-export([compile/1]).
-compile(export_all).

atom_to_var(Name) when is_atom(Name) ->
    [FirstChar|Tl] = erlang:atom_to_list(Name),
    case FirstChar of
        LowerCase when LowerCase > 96 andalso LowerCase < 123 ->
            [LowerCase-32|Tl];
        _ ->
            [FirstChar|Tl]
    end.


getter_ast(Modulename, #'FIELD'{name = FunctionName}) ->
    erl_syntax:function(
      erl_syntax:atom(FunctionName),
      [erl_syntax:clause(
         [erl_syntax:variable(atom_to_var(Modulename))], none,
         [erl_syntax:record_access(erl_syntax:variable(atom_to_var(Modulename)), erl_syntax:atom(Modulename), erl_syntax:atom(FunctionName))])]).

setter_ast(Modulename, #'FIELD'{name = FunctionName}) ->
    erl_syntax:function(
      erl_syntax:atom(FunctionName),
      [erl_syntax:clause(
         [erl_syntax:variable(atom_to_var(FunctionName)), erl_syntax:variable(atom_to_var(Modulename))], none,
         [erl_syntax:record_expr(
            erl_syntax:variable(atom_to_var(Modulename)),
            erl_syntax:atom(Modulename),
            [erl_syntax:record_field(
              erl_syntax:atom(FunctionName),
              erl_syntax:variable(atom_to_var(FunctionName))
             )])])]).


create_record_from_fields(Modulename, Fields) ->
    RecordFields =
        lists:map(
          fun(#'FIELD'{name = Name}) ->
                  erl_syntax:record_field(erl_syntax:atom(Name))
          end,
          Fields),

    erl_syntax:attribute(
      erl_syntax:atom(record),
      [erl_syntax:atom(Modulename),
       erl_syntax:tuple(RecordFields)]
     ).

create_new_function(ModuleName, Fields) ->
    Args =
        lists:map(
          fun(#'FIELD'{name = Name}) ->
                  erl_syntax:variable(atom_to_var(Name))
          end,
          Fields),

    FieldSetters =
        lists:map(
          fun(#'FIELD'{name = Name}) ->
                  erl_syntax:record_field(erl_syntax:atom(Name), erl_syntax:variable(atom_to_var(Name)))
          end,
          Fields),
    erl_db_creator_helpers:function("new", Args, none, [erl_syntax:record_expr(none, erl_syntax:atom(ModuleName), FieldSetters)]).


compile(#'MODEL'{imports = Imports, name = {Name, _NameLine}, backend = {Backend, _BackendLine}, fields = Fields, functions = Functions}) ->
    ModuleAST = erl_db_creator_helpers:module(Name),
    BackendGetFunctionAST =
        erl_db_creator_helpers:function("backend", [], none, [erl_syntax:atom(Backend)]),
    BackendGetFunction1AST =
        erl_db_creator_helpers:function("backend", [erl_syntax:underscore()], none, [erl_syntax:atom(Backend)]),

    FieldsfunctionAST =
        erl_db_creator_helpers:function("fields", [], none, [erl_db_creator_helpers:proplist_from_fields(Fields)]),

    Fieldsfunction2AST =
        erl_db_creator_helpers:function("fields", [erl_syntax:underscore()], none, [erl_db_creator_helpers:proplist_from_fields(Fields)]),

    GettersAST = [ getter_ast(Name, Field) || Field <- Fields ],
    SettersAST = [ setter_ast(Name, Field) || Field <- Fields ],

    NewFunctionAST = create_new_function(Name, Fields),

    FunctionRecordAST = create_record_from_fields(Name, Fields),

    CompileAST = erl_syntax:attribute(erl_syntax:atom(compile), [erl_syntax:atom("export_all")]),

    Forms = [ erl_syntax:revert(AST) || AST <- [ModuleAST, CompileAST, FunctionRecordAST, BackendGetFunctionAST, BackendGetFunction1AST, NewFunctionAST, FieldsfunctionAST, Fieldsfunction2AST] ++ GettersAST ++ SettersAST ],

    case compile:forms(Forms) of
        {ok,ModuleName,Binary} ->
            FileName = atom_to_list(ModuleName) ++ ".beam",
            file:write_file(FileName, Binary),
            {ok, FileName};
        {ok,ModuleName,Binary,Warnings} ->
            erl_db_log:msg(warning, "Compiled ~p with warnings: ~p", [ModuleName, Warnings]),
            FileName = atom_to_list(ModuleName) ++ ".beam",
            file:write_file(FileName, Binary),
            {ok, FileName};
        {error, Errors, _Warnings} ->
            erl_db_log:msg(error, "Could not compile model: ~p. Exited with errors: ~p~n", [Errors])
    end.
