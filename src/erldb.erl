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
         save/1,
        ]).

start(_Args) ->
    application:start(erldb).

stop() ->
    application:stop(erldb).

%%--------------------------------------------------------------------
%% @doc Searches for models with the given conditions
%%
%% @spec find(Model :: atom(), Conditions :: [{Field :: atom(), Op :: atom(), Val :: any()}]) -> list()
%% @end
%%--------------------------------------------------------------------
find(Model, Conditions) ->
    find(Model, Conditions, []).

find(Model, Conditions, Options) ->
    Attributes = Model:module_info(attributes),
    [Poolnames|_] = proplists:get_value(backend, Attributes, []),
    lists:foldl(
      fun({Poolname, _Args}, Results) ->
              Worker = poolboy:checkout(Poolname),
              case gen_server:call(Worker, {supported_condition, Conditions}) of
                  ok ->
                      {ok, Res} = gen_server:call(Worker, {find, Model, Conditions, Options}),
                      poolboy:checkin(Poolname, Worker),
                      Res;
                  {not_supported, Operator} ->
                      erldb_log:msg(error, "'~p' does not support query operator '~p'", [Model, Operator]),
                      poolboy:checkin(Poolname, Worker),
                      Results
              end
      end,
      [], Poolnames).

%%--------------------------------------------------------------------
%% @doc Deletes the specified 'Object'.
%% @spec delete(Object :: tuple()) -> {ok, DeletedObjects :: integer()}
%% @end
%%--------------------------------------------------------------------
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
            lists:foldl(
              fun({Poolname, Arguments}, Results) ->
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
%% @spec save(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
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
%% @spec update(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
%% @end
%%--------------------------------------------------------------------
-spec update(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
update(Object) when is_tuple(Object) ->
    Module = elemnent(1, Object),
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
                    lists:foldl(
                      fun({Poolname, Arguments}, Results) ->
                              Worker = poolboy:checkout(Poolname),
                              Res = gen_server:call(Worker, {update, NewObject, Arguments}),
                              poolboy:checkin(Poolname, Worker),
                              Res
                      end, proplists:get_value(backend, Module:module_info(attributes, []))),
                lists:any(fun({ok, _}) -> true; (_) -> false end, Res),
            _ ->
                []
        end,

    case {Proceed, erlang:function_exported(Module, '_post_update', 1)} of
        {stop, _} ->
            {stopped, Object};
        {ok, true} ->
            Module:'_post_update'(NewObject, UpdateRes);
        {_, {error, Reason}} ->
            {error, Reason};
        _ ->
            {ok, New}
    end.

%%--------------------------------------------------------------------
%% @doc Performs an insert operation for an object
%% @spec insert(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
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
        end,

    case {Proceed, erlang:function_exported(Module, '_post_insert', 1)) of
        {stop, _} ->
            {stopped, Object};
        {ok, true} ->
            Module:'_post_insert'(NewObject);
        {_, {error, Reason}} ->
            {error, Reason};
        _ ->
            {ok, NewObject}
    end.
