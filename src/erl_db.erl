-module(erl_db).

-export([
         start/1,
         stop/0,
         find/2,
         delete/1,
         delete/2,
         save/1
        ]).

start(_Args) ->
    application:start(erl_db).

stop() ->
    application:stop(erl_db).

find(Model, Conditions) ->
    Attributes = Model:module_info(attributes),
    Poolnames = proplists:get_value(backend, Attributes, []),
    lists:foldl(
      fun({Poolname, _Args}, Results) ->
              Worker = poolboy:checkout(Poolname),
              Res = gen_server:call(Worker, {find, Model, normalize_conditions(Conditions)}),
              poolboy:checkin(Poolname, Worker),
              Res ++ Results
      end,
      [], Poolnames).

delete(Object) when is_tuple(Object) ->
    %% Let's build ourself the conditons
    Model = element(1, Object),
    Fields = proplists:get_value(fields, Model:module_info(attributes), []),
    [_|Values] = erlang:tuple_to_list(Object),
    Conditions = lists:zipwith(fun({Fieldname, _, _}, Value) ->
                                       {Fieldname, Value}
                               end, Fields, Values),
    io:format("delete(~p, ~p)~n", [Model, Conditions]),
    delete(Model, Conditions).

delete(Model, Conditions) when is_atom(Model) ->
    Poolnames = proplists:get_value(backend, Model:module_info(attributes), []),
    lists:foreach(
      fun({Poolname, _Args}) ->
              Worker = poolboy:checkout(Poolname),
              gen_server:call(Worker, {delete, Model, normalize_conditions(Conditions)}),
              poolboy:checkin(Poolname, Worker)
      end, Poolnames).


save(Model) when is_tuple(Model) ->
    Modelname = element(1, Model),
    Attributes = Modelname:module_info(attributes),
    [{Poolname, _PoolArgs}] = proplists:get_value(backend, Attributes),

    Worker = poolboy:checkout(Poolname),
    Res = gen_server:call(Worker, {save, Model}),
    poolboy:checkin(Poolname, Worker),
    Res.

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
