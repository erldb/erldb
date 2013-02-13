-module(erl_db_creator).

-include("../include/erl_db_types.hrl").

-export([create/1]).

validator([]) ->
    none;
validator({Type, nil}) ->
    %% Only check for type
    GuardFunc =
        case Type of
            'string' ->
                is_list;
            'integer' ->
                is_integer;
            'binary' ->
                is_binary;
            'float' ->
                is_float;
            'datetime' ->
                is_tuple;
            'timestamp' ->
                is_tuple;
            'boolean' ->
                is_boolean;
            _ ->
                exit
        end,

    case GuardFunc of
        exit ->
            [];
        _ ->
            erl_syntax:application(
              erl_syntax:atom(erlang),
              erl_syntax:atom(GuardFunc),
              [erl_syntax:variable("Value")])
    end;

validator({Type, Length}) ->
    SyntaxGuard = [validator({Type, nil})],
    LengthGuard =
        erl_syntax:infix_expr(
          erl_syntax:application(
            erl_syntax:atom(erlang),
            erl_syntax:atom(length),
            [erl_syntax:variable("Value")]),
          erl_syntax:operator('<'),
          erl_syntax:integer(Length)),
    [LengthGuard|SyntaxGuard].

export_ast(FunctionName, Arity) ->
    erl_syntax:arity_qualifier(
      erl_syntax:atom(FunctionName),
      erl_syntax:integer(Arity)).

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

setter_ast(FunctionName, Validator) ->
    erl_syntax:function(erl_syntax:atom(FunctionName),
                        [erl_syntax:clause(
                           [erl_syntax:variable("Value")],
                           validator(Validator),
                           [erl_syntax:application(
                              erl_syntax:atom(erlang),
                              erl_syntax:atom(put),
                              [erl_syntax:atom(FunctionName), erl_syntax:variable("Value")])])]).

parse_fields([], State) ->
    State;
parse_fields([#'FIELD'{name = {identifier, Name, _TokenLine, _TokenLen}, validator = Validator, line = Line}|Tl], {Exports, Functions}) ->
    %% Getter export
    GetterExport = export_ast(Name, 0),
    %% Setter export
    SetterExport = export_ast(Name, 1),

    %% Getter function
    GetterFunction = getter_ast(Name),

    %% Setter function
    SetterFunction = setter_ast(Name, Validator),

    parse_fields(Tl, {[GetterExport, SetterExport|Exports], [GetterFunction, SetterFunction|Functions]}).

create(#'MODEL'{name = Name, backend = Backend, fields = Fields, functions = Functions}) ->
    ModuleAST = erl_syntax:attribute(erl_syntax:atom(module),
                                     [erl_syntax:atom(Name)]),

    BackendGetFunctionAST = erl_syntax:function(erl_syntax:atom("backend"),
                                                [erl_syntax:clause(
                                                   [], none,
                                                   [erl_syntax:atom(Backend)])]),

    {FieldsExportAST, FunctionsAST} = parse_fields(Fields, {[], [BackendGetFunctionAST]}),

    ExportAST = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list(
                                        [erl_syntax:arity_qualifier(
                                           erl_syntax:atom("backend"),
                                           erl_syntax:integer(0))|FieldsExportAST])]),

    Forms = [ erl_syntax:revert(AST) || AST <- [ModuleAST, ExportAST] ++ FunctionsAST],

    case compile:forms(Forms) of
        {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "z", Binary);
        {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "z", Binary);
        P -> P
    end.
