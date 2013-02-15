-module(erl_db_creator_helpers).

-include("../include/erl_db_types.hrl").

-compile(export_all).

valid_type(Type, Line) ->
    case Type of
        id ->
            ok;
        string ->
            ok;
        binary ->
            ok;
        integer ->
            ok;
        float ->
            ok;
        datetime ->
            ok;
        timestamp ->
            ok;
        boolean ->
            ok;
        primary_key ->
            ok;
        foreign_key ->
            ok;
        one_to_many ->
            ok;
        _ ->
            {error, unknown_type, Type, Line}
    end.

module(Modulename) ->
    erl_syntax:attribute(erl_syntax:atom(module),
                         [erl_syntax:atom(Modulename)]).

export_fun(FunctionName, Arity) ->
    erl_syntax:arity_qualifier(
      erl_syntax:atom(FunctionName),
      erl_syntax:integer(Arity)).

function(Functionname, Arguments, Guards, Body) ->
    erl_syntax:function(erl_syntax:atom(Functionname),
                        [erl_syntax:clause(
                           Arguments, Guards,
                           Body)]).


convert_type(Type) when is_atom(Type) ->
    erl_syntax:atom(Type);
convert_type(Type) when is_integer(Type) ->
    erl_syntax:integer(Type);
convert_type(_) ->
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

proplist_from_fields(#'FIELD'{name = Name, type = Type, arguments = Args, line = Line}) ->
    ok = valid_type(Type, Line),
    erl_syntax:tuple([erl_syntax:atom(Name), erl_syntax:atom(Type), erl_syntax:list(convert_argument(Args))]).
