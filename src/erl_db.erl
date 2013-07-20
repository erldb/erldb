-module(erl_db).

-export([find/2,
         delete/2,
         save/1,
         create_table/1]).

find(Model, Conditions) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {find, Model, normalize_conditions(Conditions)})
                                  end).

delete(Model) when is_tuple(Model) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {delete, Model})
                                  end).

delete(Model, Conditions) when is_atom(Model) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {delete, Model, normalize_conditions(Conditions)})
                                  end).

save(Model) when is_tuple(Model) ->
    Modelname = element(1, Model),
    Attributes = Modelname:module_info(attributes),
    [Poolname] = proplists:get_value(backend, Attributes),

    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {save, Model})
                                  end).

create_table(Model) ->
    PoolName = Model:backend(),
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {create_table, Model})
                        end).

transaction(Model) ->
    PoolName = Model:backend(),
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {transaction, Model})
                        end).


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
