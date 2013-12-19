%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se> [http://www.burbas.se]
%%% @copyright (C) 2013, Niclas Axelsson
%%% @doc
%%% Main interface for erldb
%%% @end
%%% Created : 16 Dec 2013 by Niclas Axelsson
%%%-------------------------------------------------------------------
-module(erldb).
-export([
         start/1,
         stop/0,
         find/2,
         delete/1,
         save/1
        ]).

%%--------------------------------------------------------------------
%% @doc Starts the erldb application
%%
%% @end
%%--------------------------------------------------------------------
start(_Args) ->
    application:start(erldb).

%%--------------------------------------------------------------------
%% @doc Stops the erldb application
%%
%% @end
%%--------------------------------------------------------------------
stop() ->
    application:stop(erldb).

%%--------------------------------------------------------------------
%% @doc Searches for models with the given conditions
%%
%% @end
%%--------------------------------------------------------------------
-spec find(atom(), [tuple()]) -> {ok, list()} | {error, atom()}.
find(Model, Conditions) ->
    find(Model, Conditions, []).

find(Model, Conditions, Options) ->
    Attributes = Model:module_info(attributes),
    [{Poolname, _Args}|_] = proplists:get_value(backend, Attributes, []),

    Worker = poolboy:checkout(Poolname),
    Result =
        case gen_server:call(Worker, {supported_condition, Conditions}) of
            ok ->
                gen_server:call(Worker, {find, Model, Conditions, Options});
            {not_supported, Operator} ->
                erldb_log:msg(error, "'~p' does not support query operator '~p'", [Model, Operator]),
                {error, op_not_supported}
        end,
    poolboy:checkin(Poolname, Worker),
    Result.

%%--------------------------------------------------------------------
%% @doc Deletes the specified 'Object'.
%% @end
%%--------------------------------------------------------------------
-spec delete(tuple()) -> ok.
delete(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    Proceed =
        case erlang:function_exported(Module, '_pre_delete', 1) of
            true ->
                case Module:'_pre_delete'(Object) of
                    stop ->
                        stop;
                    _ ->
                        ok
                end;
            _ ->
                ok
        end,

    case Proceed of
        ok ->
            lists:map(
              fun({Poolname, Arguments}) ->
                      Worker = poolboy:checkout(Poolname),
                      Res = gen_server:call(Worker, {delete, Object, Arguments}),
                      poolboy:checkin(Poolname, Worker),
                      Res
              end, proplists:get_value(backend, Module:module_info(attributes, [])));
        _ ->
            ok
    end,

    case {Proceed, erlang:function_exported(Module, '_post_delete', 1)} of
        {stop, _} ->
            ok;
        {_, true} ->
            case Module:'_post_delete'(Object) of
                rollback ->
                    Object:save(); %% Save the object back to database
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Saves the specified 'Object' to the database.
%% @end
%%--------------------------------------------------------------------
-spec save(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
save(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    %% Determine if this is an insert or an update
    [PrimaryKey] =
        lists:filter(
          fun({field, [{_, _, _, Arglist}]}) -> proplists:get_value(primary_key, Arglist, false); (_) -> false end,
          Module:module_info(attributes)),
    PrimaryKeyPos = element(2, PrimaryKey),
    case element(PrimaryKeyPos, Object) of
        'id' ->
            %% This is an insertion
            insert(Object);
        _ ->
            update(Object)
    end.

%%--------------------------------------------------------------------
%% @doc Performs an update operation for an object
%% @end
%%--------------------------------------------------------------------
-spec update(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
update(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    {Proceed, NewObject} =
        case erlang:function_exported(Module, '_pre_update', 1) of
            true ->
                case Module:'_pre_update'(Object) of
                    stop ->
                        {stop, undefined};
                    {ok, Obj} ->
                        {ok, Obj}
                end;
            _ ->
                {ok, Object}
        end,

    UpdateRes =
        case Proceed of
            {ok, _} ->
                Res =
                    lists:map(
                      fun({Poolname, Arguments}) ->
                              Worker = poolboy:checkout(Poolname),
                              Res = gen_server:call(Worker, {update, NewObject, Arguments}),
                              poolboy:checkin(Poolname, Worker),
                              Res
                      end, proplists:get_value(backend, Module:module_info(attributes, []))),
                lists:any(fun({ok, _}) -> true; (_) -> false end, Res);
            _ ->
                []
        end,

    case {Proceed, erlang:function_exported(Module, '_post_update', 1)} of
        {stop, _} ->
            {stopped, Object};
        {ok, true} ->
            Module:'_post_update'(NewObject, UpdateRes);
        _ ->
            {ok, NewObject}
    end.

%%--------------------------------------------------------------------
%% @doc Performs an insert operation for an object
%% @end
%%--------------------------------------------------------------------
-spec insert(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
insert(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    {Proceed, NewObject} =
        case erlang:function_exported(Module, '_pre_insert', 1) of
            true ->
                case Module:'_pre_insert'(Object) of
                    stop ->
                        {stop, undefined};
                    _ ->
                        %% @TODO Update the object. Should return in format {ok, Object} or {error, Reason}
                        ok
                end;
            _ ->
                %% @TODO Update the object. Should return in format {ok, Object} or {error, Reason}
                ok
        end,

    case {Proceed, erlang:function_exported(Module, '_post_insert', 1)} of
        {stop, _} ->
            {stopped, Object};
        {ok, true} ->
            Module:'_post_insert'(NewObject);
        {_, {error, Reason}} ->
            {error, Reason};
        _ ->
            {ok, NewObject}
    end.
