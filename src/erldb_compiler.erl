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
