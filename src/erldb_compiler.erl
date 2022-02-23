%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se> [http://www.novaframework.org]
%%% @doc
%%% Simple compiler for erldb models
%%% @end
%%%-------------------------------------------------------------------
-module(erldb_compiler).

-export([
         parse_transform/2
        ]).

-record(model_state, {
                      name = "" :: string(),
                      fields = [] :: [tuple()],
                      attributes = [] :: [tuple()],
                      body = [] :: [tuple()],
                      relations = [] :: [tuple()],
                      fc = 1 :: integer() %% Field counter. Starts on 1 since 1 contains the record-name
                     }).

-define(INDENT_WS, "    ").

%%--------------------------------------------------------------------
%% @doc Compiles a file with additional options.
%% Options :: #{outdir := Path,
%%              includedir := Path}
%% @end
%%--------------------------------------------------------------------
-spec parse_transform(Form :: [tuple()], Options :: list()) -> {ok, Beamfile :: binary()} | ok | {error, Error :: atom()}.
compile(Form, _Options) ->
    Form.

%%--------------------------------------------------------------------
%% @doc Divides all the forms into lists grouped by type.
%% @end
%%--------------------------------------------------------------------
-spec post_parse(tuple(), #model_state{}) -> #model_state{}.
post_parse({attribute, R0, field, {Name, Type}}, State) ->
    post_parse({attribute, R0, field, {Name, Type, []}}, State);
post_parse({attribute, R0, field, {Name, Type, Arguments}}, State = #model_state{
                                                                       attributes = Attributes,
                                                                       fields = Fields,
                                                                       fc = FC}) ->
    A = {attribute, R0, field, {Name, FC, Type, Arguments}},
    State#model_state{fields = [{Name, Type, Arguments}|Fields], attributes = [A|Attributes], fc = FC-1};
post_parse({attribute, R0, relation, {belongs_to, Model}}, State = #model_state{
                                                                      fields = Fields,
                                                                      fc = FC,
                                                                      attributes = Attributes,
                                                                      relations = Relations}) ->
    %% We need to define a new attribute which contains the primary key of the other model.
    %% This is a bit tricky since we need to check which field is the primary one.
    PrimaryKey =
        case code:is_loaded(Model) of
            false ->
                %% Here's when it gets tricky, should we try and compile the model or just simply fail?
                %% If we compile we might get into a infinite loop if circular dependencies exists
                ok;
            _ ->
                %% Get the primary key type
                ok
        end,
    Fieldname = erlang:list_to_atom(io_lib:format("~s_id", [Model])),
    A = {attribute, R0, field, {Fieldname, FC, PrimaryKey, []}},
    State#model_state{fields = [{Fieldname, PrimaryKey, []}|Fields],
                      attributes = [A|Attributes],
                      fc = FC-1,
                      relations = [{belongs_to, Model}|Relations]};
post_parse({attribute, _R0, relation, {has, Amount, Model}}, State = #model_state{
                                                                        relations = Relations}) ->
    %% This model is linked to 'Amount' numbers of rows within another model
    %% TODO: check which field is the primary_key
    State#model_state{relations = [{has, Amount, Model}|Relations]};
post_parse(Element, State = #model_state{body = Body}) ->
    State#model_state{body = [Element|Body]}.

%%--------------------------------------------------------------------
%% @doc Fixes the attributes so it can be handled by erl_parse.
%% @end
%%--------------------------------------------------------------------
-spec pre_parse([Tokens], false | {true, [Tokens]}) -> [Tokens].
pre_parse([], _) -> [];
pre_parse([{'-', _LN0} = T1, {atom, _, _} = T2, {'(', L3} = T3|Tl], false) ->
    pre_parse(Tl, {true, [{'{', L3}, T3, T2, T1]});
pre_parse([{')', L0} = T0|Tl], {true, Attr}) ->
    lists:reverse([T0, {'}', L0}|Attr]) ++ pre_parse(Tl, false);
pre_parse([Hd|Tl], {true, Attr}) ->
    pre_parse(Tl, {true, [Hd|Attr]});
pre_parse([Hd|Tl], false) ->
    [Hd|pre_parse(Tl, false)].

%%--------------------------------------------------------------------
%% @doc Rebuilds all the functions to accept record-definition as argument.
%% @end
%%--------------------------------------------------------------------
-spec rebuild_functions([tuple()], string(), [tuple()]) -> [tuple()].
rebuild_functions([], _, _) ->
    [];
rebuild_functions(Functions, Modelname, Fields) ->
    FieldAccessor =
        erl_syntax:record_expr(none, erl_syntax:atom(Modelname),
                               [erl_syntax:record_field(
                                  erl_syntax:atom(Field),
                                  erl_syntax:variable(atom_to_var(Field)))
                                || {Field, _, _} <- Fields]),
    FieldAccessor2 = [erl_syntax:revert(FieldAccessor)],

    lists:map(
      fun(Func) ->
              rebuild_function(Func, FieldAccessor2)
      end, Functions).

%%--------------------------------------------------------------------
%% @doc Rebuilds function to accept record-definition as argument.
%% @end
%%--------------------------------------------------------------------
-spec rebuild_function(tuple(), [tuple()]) -> tuple().
rebuild_function({function, F1, FunName, Arity, [{clause, C1, Args, Guards, Body}]}, FieldAccessors) ->
    {function, F1, FunName, Arity+1, [{clause, C1, Args ++ FieldAccessors, Guards, Body}]};
rebuild_function(Other, _) ->
    Other.

%%--------------------------------------------------------------------
%% @doc Generates a record definition from a set of fields.
%% @end
%%--------------------------------------------------------------------
-spec generate_hrl(atom(), [{atom(), atom(), [{atom(), any()}|atom()]}]) -> string().
generate_hrl(Modelname, Fields) ->
    io_lib:format("-record(~s, {~n~s~n}).", [Modelname, generate_hrl_fields(Fields)]).


-spec generate_hrl_fields([{atom(), atom(), [tuple()]}]) -> string().
generate_hrl_fields([]) -> [];
generate_hrl_fields([{Name, Type, Args}|Tl]) ->
    Res =
        case proplists:get_value(default, Args) of
            undefined ->
                case proplists:get_value(primary_key, Args) of
                    undefined ->
                        io_lib:format("~s~s :: undefined | ~s", [?INDENT_WS, Name, convert_to_erl_type(Type)]);
                    _ ->
                        io_lib:format("~s~s = id :: id | ~s", [?INDENT_WS, Name, convert_to_erl_type(Type)])
                end;
            DefaultValue ->
                io_lib:format("~s~s = \"~s\" :: ~s", [?INDENT_WS, Name, DefaultValue, convert_to_erl_type(Type)])
        end,
    case length(Tl) of
        0 ->
            io_lib:format("~s~n~s", [Res, generate_hrl_fields(Tl)]);
        _ ->
            io_lib:format("~s,~n~s", [Res, generate_hrl_fields(Tl)])
    end.

%%--------------------------------------------------------------------
%% @doc Builds relation get-functions
%% @end
%%--------------------------------------------------------------------
-spec build_relation_functions(CurrentModel :: string(), [tuple()]) -> [] | [tuple()].
build_relation_functions(_, []) ->
    [];
build_relation_functions(CurrentModel, [{has, Amount, Model}|Tl]) ->
    FuncName = case Amount of
                   1 -> Model;
                   _ -> inflector:pluralize(erlang:atom_to_list(Model))
               end,
    %% Generates function: FuncName(Model) -> erldb:find(Model, [{CurrentModel_id, Model#CurrentModel.id}]).
    [erl_syntax:function(
       erl_syntax:atom(FuncName),
       [erl_syntax:clause(
          [erl_syntax:variable("Model")], none, [erl_syntax:application(
                                                   erl_syntax:atom(erldb),
                                                   erl_syntax:atom(find),
                                                   [ erl_syntax:atom(Model),
                                                     erl_syntax:list([
                                                                      erl_syntax:tuple([
                                                                                        erl_syntax:atom(io_lib:format("~s_id", [CurrentModel])),
                                                                                        erl_syntax:record_access(erl_syntax:variable("Model"),
                                                                                                                 erl_syntax:atom(CurrentModel),
                                                                                                                 erl_syntax:atom("id"))
                                                                                       ])])
                                                   ])])])|build_relation_functions(CurrentModel, Tl)];
build_relation_functions(CurrentModel, [_|Tl]) ->
    build_relation_functions(CurrentModel, Tl).

%%--------------------------------------------------------------------
%% @doc Generates the resulting beam-file in the file system
%% @end
%%--------------------------------------------------------------------
-spec generate_beam(#compiler_state{}, RecordDefinition :: [tuple()]) -> {ok, Beamfile :: binary()} | ok.
generate_beam(#compiler_state{outdir = OutDir,
                              model_state = #model_state{
                                               name = Name,
                                               fields = Fields,
                                               attributes = Attributes,
                                               relations = Relations,
                                               body = Body}},
              RecordDefinition) ->
    RelationFunctions = build_relation_functions(Name, Relations),
    Functions = rebuild_functions(Body, Name, Fields),
    %% Give the module a name
    AST =
        [
         %% -module(Name).
         erl_syntax:attribute(erl_syntax:atom(module),
                              [erl_syntax:atom(Name)]),
         %% -compile(export_all).
         erl_syntax:attribute(erl_syntax:atom(compile), [erl_syntax:atom("export_all")]),

         %% -include_lib("erldb/include/erldb.hrl").
         erl_syntax:attribute(erl_syntax:atom(include_lib), [erl_syntax:string("erldb/include/erldb.hrl")]),

         %% Record definition
         RecordDefinition
        ] ++ Attributes ++ [

                            %% Save function
                            erl_syntax:function(
                              erl_syntax:atom("save"),
                              [erl_syntax:clause(
                                 [erl_syntax:variable("Model")], none, [erl_syntax:application(
                                                                          erl_syntax:atom(erldb),
                                                                          erl_syntax:atom(save),
                                                                          [ erl_syntax:variable("Model") ])])]),
                            %% Delete function
                            erl_syntax:function(
                              erl_syntax:atom("delete"),
                              [erl_syntax:clause(
                                 [erl_syntax:variable("Model")], none, [erl_syntax:application(
                                                                          erl_syntax:atom(erldb),
                                                                          erl_syntax:atom(delete),
                                                                          [ erl_syntax:variable("Model") ])])])

                           ] ++ Functions ++ RelationFunctions,
    Forms = [ erl_syntax:revert(Form) || Form <- AST ],
    case compile:forms(Forms, [report_errors]) of
        {ok, ModuleName, Binary} ->
            logger:info("Compiled model ~s", [ModuleName]),
            BeamFile = io_lib:format("~s.beam", [ModuleName]),
            ok = file:write_file(filename:join(OutDir, BeamFile), Binary),
            {ok, ModuleName};
        Error ->
            logger:error("Could not compile model ~s, reason: ~p", [Name, Error])
    end.
q
%%--------------------------------------------------------------------
%% @doc Splits a list of tokens into list with a single form.
%% @end
%%--------------------------------------------------------------------
-spec split_on_dot([Tokens], [Tokens], [Tokens]) -> [Tokens].
split_on_dot([], Acc, _CurrentDot) ->
    Acc;
split_on_dot([Hd={dot, _}|Tl], Acc, CurrentDot) ->
    split_on_dot(Tl, [lists:reverse([Hd|CurrentDot])|Acc], []);
split_on_dot([Hd|Tl], Acc, CurrentDot) ->
    split_on_dot(Tl, Acc, [Hd|CurrentDot]).

%%--------------------------------------------------------------------
%% @doc Converts a type from the model specification to an erlang dito.
%% @end
%%--------------------------------------------------------------------
-spec convert_to_erl_type(atom()) -> string().
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
convert_to_erl_type(_Type) ->
    "any()".


atom_to_var(Atom) when is_atom(Atom) ->
    atom_to_var(atom_to_list(Atom));
atom_to_var([FirstChar|Tl]) when FirstChar > 96 andalso FirstChar < 123 ->
    [FirstChar-32|Tl];
atom_to_var(String) when is_list(String) ->
    String.
