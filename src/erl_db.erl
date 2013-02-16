-module(erl_db).

-export([info/1]).

info(PoolName) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, info)
    end).


find(Model, Id) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {find, Model, Id})
                                  end).

delete(Model) when is_tuple(Model) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {delete, Model})
                                  end).

save(Model) when is_tuple(Model) ->
    Poolname = Model:backend(),
    poolboy:transaction(Poolname, fun(Worker) ->
                                          gen_server:call(Worker, {save, Model})
                                  end).
