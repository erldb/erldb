
-module(erl_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Pools = erl_db_env:get_env(erl_db, db_pools, []),
    PoolSpecs = lists:map(fun({Name, Type, SizeArgs, WorkerArgs}) ->
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, Type},
                                              {worker_args, [{db_name, Name}, {db_type, Type}|WorkerArgs]}
                                             ] ++ SizeArgs,
                                  {Name, {poolboy, start_link, [PoolArgs]}, permanent, 5000, worker, [poolboy]}
                          end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
