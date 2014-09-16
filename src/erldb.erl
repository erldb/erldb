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
         find/3,
         find_one/2,
         find_one/3,
         delete/1,
         delete/2,
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
    [{Poolname, _Args}|_] = get_backends(Model),
    NConditions = normalize_conditions(Conditions),
    Worker = poolboy:checkout(Poolname),
    Result =
        case gen_server:call(Worker, {supported_condition, NConditions}) of
            {ok, supported} ->
                gen_server:call(Worker, {find, Model, NConditions, Options});
            {error, not_supported, Operator} ->
                io:format("'~p' does not support query operator: ~p~n", [Model, Operator]),
                {error, op_not_supported}
        end,
    poolboy:checkin(Poolname, Worker),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% Finds one
%% @end
%%--------------------------------------------------------------------
-spec find_one(atom(), [tuple()]) -> tuple() | not_found.
find_one(Model, Conditions) ->
    find_one(Model, Conditions, []).

-spec find_one(atom(), [tuple()], [tuple()]) -> tuple() | not_found.
find_one(Model, Conditions, Options) ->
    [{Poolname, _Args}|_] = get_backends(Model),
    Worker = poolboy:checkout(Poolname),
    Result =
        case gen_server:call(Worker, {supported_operation, find_one}) of
            {error, op_not_supported} ->
                case find(Model, Conditions, Options) of
                    {ok, [E|_]} ->
                        E;
                    _ ->
                        not_found
                end;
            {ok, supported} ->
                gen_server:call(Worker, {find_one, Model, Conditions, Options})
        end,
    poolboy:checkin(Poolname, Worker),
    Result.

%%--------------------------------------------------------------------
%% @doc Deletes the specified 'Object'.
%% @end
%%--------------------------------------------------------------------
-spec pre_delete(atom(), tuple()) -> stop | ok.
pre_delete(Module, Object) ->
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
    end.

-spec post_delete(atom(), tuple()) -> stop | ok.
post_delete(Module, Object) ->
    case erlang:function_exported(Module, '_post_delete', 1) of
        false ->
            ok;
        true ->
            case Module:'_post_delete'(Object) of
                rollback ->
                    Object:save(); %% Save the object back to database
                _ ->
                    ok
            end
    end.

-spec delete(atom(), [{atom(), atom(), any()}|{atom(), any()}]) -> ok | [ok].
delete(Model, Conditions) ->
    case find(Model, Conditions) of
        {ok, []} ->
            ok;
        {ok, List} ->
            [ delete(X) || X <- List ]
    end.

-spec delete(tuple()) -> ok.
delete(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    Proceed = pre_delete(Module, Object),
    case Proceed of
        ok ->
            from_all_backends(delete, Object);
        _ ->
            ok
    end,
    post_delete(Module, Object).

%%--------------------------------------------------------------------
%% @doc Saves the specified 'Object' to the database.
%% @end
%%--------------------------------------------------------------------
-spec save(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
save(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    %% Determine if this is an insert or an update
    PrimaryKeyPos = primary_key_pos(Module:module_info(attributes)),
    case element(PrimaryKeyPos, Object) of
        'id' ->
            %% This is an insertion
            case insert(Object) of
                {stopped, Obj} ->
                    {stopped, Obj};
                Res ->
                    Res
            end;
        _Value ->
            update(Object)
    end.

primary_key_pos([]) ->
    {error, not_found};
primary_key_pos([{field, [{_Fieldname, Pos, _Type, Arglist}]}|T]) ->
    case lists:member(primary_key, Arglist) of
        true ->
            Pos;
        _ ->
            primary_key_pos(T)
    end;
primary_key_pos([_|T]) ->
    primary_key_pos(T).

%%--------------------------------------------------------------------
%% @doc Performs a pre-update hook for an object
%% Pre-update is called, if exists, right before the update is sent to the
%% underlying adapter. If the hook returns the atom 'stop' the whole operation
%% is aborted and the object will not be updated.
%% @end
%%--------------------------------------------------------------------
-spec pre_update(Module :: atom(), Object :: tuple()) -> {ok, Object :: tuple()} | {stop, undefined}.
pre_update(Module, Object) ->
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
    end.


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update(tuple()) -> {ok, tuple()} | {stopped, tuple()} | {error, atom()}.
update(Object) when is_tuple(Object) ->
    Module = element(1, Object),
    {Proceed, NewObject} = pre_update(Module, Object),
    UpdateRes =
        case Proceed of
            ok ->
                Res = from_all_backends(update, Object),
                lists:any(fun({ok, _}) -> true; (_) -> false end, Res);
            stop ->
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
                    {ok, Obj} ->
                        {ok, Obj}
                end;
            _ ->
                {ok, Object}
        end,

    case {Proceed, erlang:function_exported(Module, '_post_insert', 1)} of
        {stop, _} ->
            {stopped, Object};
        {ok, true} ->
            case Module:'_post_insert'(NewObject) of
                stop ->
                    {stopped, Object};
                {ok, NewObject2} ->
                    from_all_backends(save, NewObject2)
            end;
        _ ->
            from_all_backends(save, NewObject)
    end.


from_all_backends(Action, Object) ->
    _List = lists:map(
              fun({Poolname, _Arguments}) ->
                      Worker = poolboy:checkout(Poolname),
                      Res = gen_server:call(Worker, {Action, Object}),
                      poolboy:checkin(Poolname, Worker),
                      Res
              end, get_backends(element(1, Object))).

get_backends(Module) ->
    case proplists:get_value(backend, Module:module_info(attributes)) of
        undefined ->
            application:get_env(default_backend);
        Backends ->
            Backends
    end.

normalize_conditions([]) -> [];
normalize_conditions([{Fieldname, Value}|Tl]) ->
    [{Fieldname, 'equals', Value}|normalize_conditions(Tl)];
normalize_conditions([Hd|Tl]) ->
    [Hd|normalize_conditions(Tl)].
