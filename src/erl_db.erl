%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% The main interface for erl_db
%%% @end
%%% Created : 26 Jul 2013 by Niclas Axelsson <niclas@burbas.se>
-module(erl_db).
-export([
         start/1,
         stop/0,
         find/2,
         delete/1,
         delete/2,
         save/1,
         get_models/1
        ]).

start(_Args) ->
    application:start(erl_db).

stop() ->
    application:stop(erl_db).

%%--------------------------------------------------------------------
%% @doc Searches for models with the given conditions
%%
%% @spec find(Model :: atom(), Conditions :: [{atom(), any()}]) -> list()
%% @end
%%--------------------------------------------------------------------
find(Model, Conditions) ->
    find(Model, Conditions, []).

find(Model, Conditions, Options) ->
    Attributes = Model:module_info(attributes),
    Poolnames = proplists:get_value(backend, Attributes, []),
    lists:foldl(
      fun({Poolname, _Args}, Results) ->
              Worker = poolboy:checkout(Poolname),
              NormalizedConditions = normalize_conditions(Conditions),
              case is_conditions_supported(Worker, NormalizedConditions) of
                  ok ->
                      Res = gen_server:call(Worker, {find, Model, normalize_conditions(Conditions), Options}),
                      poolboy:checkin(Poolname, Worker),
                      Res ++ Results;
                  {not_supported, Operator} ->
                      erl_db_log:msg(error, "'~p' does not support query operator '~p'", [Model, Operator]),
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
    Model = element(1, Object),
    Fields = proplists:get_value(fields, Model:module_info(attributes), []),
    [_|Values] = erlang:tuple_to_list(Object),
    Conditions = lists:zipwith(fun({Fieldname, _, _}, Value) ->
                                       {Fieldname, Value}
                               end, Fields, Values),
    delete(Model, Conditions).

%%--------------------------------------------------------------------
%% @doc Deletes objects of type 'Model' with the given 'conditions'
%% @spec delete(Model :: atom(), Conditions :: [{atom(), any()}]) -> {ok, DeletedObjects :: integer()}
%% @end
%%--------------------------------------------------------------------
delete(Model, Conditions) when is_atom(Model) ->
    Poolnames = proplists:get_value(backend, Model:module_info(attributes), []),
    lists:foreach(
      fun({Poolname, _Args}) ->
              Worker = poolboy:checkout(Poolname),
              NormalizedConditions = normalize_conditions(Conditions),
              case is_conditions_supported(Worker, NormalizedConditions) of
                  ok ->
                      gen_server:call(Worker, {delete, Model, NormalizedConditions});
                  {not_supported, Operator} ->
                      erl_db_log:msg(error, "Backend beloning to '~p' does not support query operator '~p'", [Model, Operator]),
                      {error, {not_supported, Operator, Model}}
              end,
              poolboy:checkin(Poolname, Worker)
      end, Poolnames).


%%--------------------------------------------------------------------
%% @doc Saves the specified 'Object' to the database.
%% @spec save(Object :: tuple()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
save(Object) when is_tuple(Object) ->
    Modelname = element(1, Object),
    Attributes = Modelname:module_info(attributes),
    [{Poolname, _PoolArgs}] = proplists:get_value(backend, Attributes),
    Worker = poolboy:checkout(Poolname),
    Res = gen_server:call(Worker, {save, Object}),
    poolboy:checkin(Poolname, Worker),
    Res.

%%--------------------------------------------------------------------
%% @doc Gets all models that's beloning to a specific, named, backend
%% @spec get_models(Backend :: atom()) -> [ Modelname :: atom() ].
%% @end
%%--------------------------------------------------------------------
get_models(Backend) when is_atom(Backend) ->
    LoadedModules = [ X || {X, _} <- code:all_loaded() ],
    lists:filter(
      fun(Modulename) ->
              case proplists:get_value(backend, Modulename:module_info(attributes)) of
                  undefined ->
                      false;
                  Backendlist ->
                      lists:any(
                        fun({ModelBackend, _}) when Backend == ModelBackend ->
                                true;
                           (_) ->
                                false
                        end, Backendlist)
              end
      end, LoadedModules).

%%---------------------------------------
%% INTERNAL FUNCTIONS
%%---------------------------------------

%%--------------------------------------------------------------------
%% @doc Checks if all 'Conditions' are supported at the targeting 'Worker'
%% @spec is_conditions_supported(Worker :: pid(), Conditions :: list() -> ok | {not_supported, Operator :: atom()}
%% @end
%%--------------------------------------------------------------------
is_conditions_supported(Worker, Conditions) when is_pid(Worker) ->
    {ok, SupportedConditions} = gen_server:call(Worker, {supported_conditions}),
    is_conditions_supported_aux(Conditions, SupportedConditions).

is_conditions_supported_aux([], _) -> ok;
is_conditions_supported_aux([{_, Operator, _}|Tl], SupportedConditions) ->
    case lists:member(Operator, SupportedConditions) of
        false ->
            {not_supported, Operator};
        true ->
            is_conditions_supported_aux(Tl, SupportedConditions)
    end.

normalize_conditions(Conditions) ->
    normalize_conditions(Conditions, []).

normalize_conditions([], Acc) ->
    lists:reverse(Acc);
normalize_conditions([Key, Operator, Value|Rest], Acc) when is_atom(Key), is_atom(Operator) ->
    normalize_conditions(Rest, [{Key, Operator, Value}|Acc]);
normalize_conditions([{Key, Value}|Rest], Acc) when is_atom(Key) ->
    normalize_conditions(Rest, [{Key, 'equals', Value}|Acc]);
normalize_conditions([{Key, 'eq', Value}|Rest], Acc) when is_atom(Key) ->
    normalize_conditions(Rest, [{Key, 'equals', Value}|Acc]);
normalize_conditions([{Key, 'ne', Value}|Rest], Acc) when is_atom(Key) ->
    normalize_conditions(Rest, [{Key, 'not_equals', Value}|Acc]);
normalize_conditions([{Key, Operator, Value}|Rest], Acc) when is_atom(Key), is_atom(Operator) ->
    normalize_conditions(Rest, [{Key, Operator, Value}|Acc]);
normalize_conditions([{Key, Operator, Value, Options}|Rest], Acc) when is_atom(Key), is_atom(Operator), is_list(Options) ->
    normalize_conditions(Rest, [{Key, Operator, Value, Options}|Acc]).
