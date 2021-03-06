-module(erldb_sup).

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
    %% Gets the environment-variable for db_pools
    {ok, Pools} = application:get_env(erldb, db_pools),

    %% Start the workers
    PoolSpecs = lists:map(fun({Name, Args}) ->
				  Type = proplists:get_value(worker, Args),
				  SizeArgs = proplists:get_value(poolboy_config, Args, 10),
				  WorkerArgs = proplists:get_value(default_table_options, Args, []),
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, Type},
                                              {worker_args, [{db_name, Name}, {db_type, Type}|WorkerArgs]}
                                             ] ++ SizeArgs,
                                  {Name, {poolboy, start_link, [PoolArgs]}, permanent, 5000, worker, [poolboy]}
                          end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
