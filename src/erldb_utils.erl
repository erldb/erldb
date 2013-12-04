%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbasconsulting.com>
%%% @doc
%%% Helper functions for ErlDB
%%% @end
%%% Created :  7 Oct 2013 by Niclas Axelsson <niclas@burbasconsulting.com>
%%%-------------------------------------------------------------------
-module(erldb_utils).

-export([
         get_fields/1,
         get_fields_with_type/2,
         set_field/3,
         set_primary_key/2,
         is_update_query/1
        ]).

get_fields(Modelname) ->
    proplists:get_value(fields, Modelname:module_info(attributes)).

get_fields_with_type(Modelname, FieldType) ->
    Fields = get_fields(Modelname),
    get_fields_with_type_aux(FieldType, Fields).

get_fields_with_type_aux(_FieldType, []) ->
    [];
get_fields_with_type_aux(FieldType, [Res={_Fieldname, FieldType, _FieldArgs, _FieldIndex}|Tl]) ->
    [Res|get_fields_with_type_aux(FieldType, Tl)];
get_fields_with_type_aux(FieldType, [_Hd|Tl]) ->
    get_fields_with_type_aux(FieldType, Tl).

get_field(_Fieldname, []) -> {error, not_found};
get_field(Fieldname, [Field = {Fieldname, _FieldType, _FieldArgs, _FieldIndex}|_Tl]) -> {ok, Field};
get_field(Fieldname, [_Hd|Tl]) -> get_field(Fieldname, Tl).


set_field(Model, Fieldname, Value) ->
    Modelname = element(1, Model),
    Fields = get_fields(Modelname),
    {ok, {_, _, _, Index}} = get_field(Fieldname, Fields),
    setelement(Index, Model, Value).


set_primary_key(Model, Value) ->
    case get_fields_with_type(element(1, Model), primary_key) of
        [] ->
            {ok, Model};
        [{_FieldName, primary_key, _FieldArgs, FieldIndex}] ->
            setelement(FieldIndex, Model, Value)
    end.

is_update_query(Model) ->
    %% Look if there's a primary key.
    case get_fields_with_type(element(1, Model), primary_key) of
        [] ->
            %% There's no primary key attribute. This is by default an insert-query
            false;
        [{_FieldName, primary_key, _FieldArgs, FieldIndex}] ->
            %% If there's more than one primary key we would have crashed a long time ago.
            case element(FieldIndex, Model) of
                'id' ->
                    %% The primary key is set to 'id'. Let's insert this one as a new element
                    false;
                _ ->
                    %% We got an index. Let's update
                    true
            end
    end.
