-module(erl_db).

-export([info/1,
         find/2,
         delete/2,
         save/1,
         create_table/1]).

info(Model) ->
    PoolName = Model:backend(),
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, info)
    end).

find(Model, Id) ->
    erl_db_log:msg(error, "test~n", []),
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {find, Model, Id})
                                  end).

delete(Model, Id) when is_tuple(Model) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {delete, Model, Id})
                                  end).

save(Model) when is_tuple(Model) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {save, Model})
                                  end).

create_table(Model) ->
    PoolName = Model:backend(),
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {create_table, Model})
                        end).