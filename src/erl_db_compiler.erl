-module(erl_db_compiler).

-include("../include/erl_db_types.hrl").

-export([compile/1]).

getter_ast(FunctionName) ->
    erl_syntax:function(
      erl_syntax:atom(FunctionName),
      [erl_syntax:clause(
         [], none,
         [erl_syntax:application(
            erl_syntax:atom(erlang),
            erl_syntax:atom(get),
            [erl_syntax:atom(FunctionName)]
           )])]).



parse_fields([], State) ->
    State;
parse_fields([#'FIELD'{name = Name, type = Type, line = Line}|Tl], {Exports, Functions}) ->
    %% Getter export
    GetterExport = erl_db_creator_helpers:export_fun(Name, 0),
    GetterFunction = erl_db_creator_helpers:function(Name, [], none, [ erl_syntax:atom("ok") ]),

    %% Setter function

    parse_fields(Tl, {[GetterExport|Exports], [GetterFunction|Functions]}).


compile(#'MODEL'{imports = Imports, name = {Name, NameLine}, backend = {Backend, BackendLine}, fields = Fields, functions = Functions}) ->
    ModuleAST = erl_db_creator_helpers:module(Name),
    BackendGetFunctionAST = erl_db_creator_helpers:function("backend", [], none, [erl_syntax:atom(Backend)]),
    FieldsfunctionAST = erl_db_creator_helpers:function("fields", [], none, [erl_db_creator_helpers:proplist_from_fields(Fields)]),

    Exports = [ erl_db_creator_helpers:export_fun(Fun, 0) || Fun <- ["backend", "fields"] ],

    {FieldsExportAST, FunctionsAST} = parse_fields(Fields, {[], [BackendGetFunctionAST, FieldsfunctionAST]}),

    ExportAST = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list(
                                        Exports)]),

    Forms = [ erl_syntax:revert(AST) || AST <- [ModuleAST, ExportAST, BackendGetFunctionAST, FieldsfunctionAST] ],

    case compile:forms(Forms) of
        {ok,ModuleName,Binary}           -> 
	    FileName = atom_to_list(ModuleName) ++ ".beam",
	    file:write_file(FileName, Binary),
	    {ok, FileName};
        {ok,ModuleName,Binary,_Warnings} ->
	    FileName = atom_to_list(ModuleName) ++ ".beam",
	    file:write_file(FileName, Binary),
	    {ok, FileName};
        P -> P
    end.
