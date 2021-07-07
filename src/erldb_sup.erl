-module(erldb_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         get_worker/1
        ]).

%% Supervisor callbacks
-export([init/1]).


-include_lib("erldb/include/erldb.hrl").

%% Helper macro for declaring children of supervisor
-define(SHUTDOWN_TIME, 4000).
-define(WORKER_ID(Name), {erldb_backend, Name}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Gets the environment-variable for backends
    Backends = application:get_env(erldb, backends, []),

    %% Get all models
    Models = erldb:get_all_models(),

    %% Start the workers
    Children = lists:map(fun({Name, Args}) ->
                                 RelevantModels = lists:filter(fun(Model) ->
                                                                       {backend, ModBackends} = proplists:lookup(backend, Model:module_info(attributes)),
                                                                       proplists:lookup(Name, ModBackends) /= none
                                                               end, Models),

                                 Module = proplists:get_value(db_module, Args),
                                 Type = proplists:get_value(sup_type, Args, worker),

                                 WorkerArgs = [
                                               {models, RelevantModels}
                                              ],

                                 ?INFO("Adding backend module ~s with identifier ~s", [Module, Name]),
                                 child_spec(Name, {Module, start_link, [WorkerArgs]}, permanent, Type)
                         end, Backends),
    {ok, {{one_for_one, 10, 10}, Children}}.

get_worker(Name) ->
    Workers = supervisor:which_children(?MODULE),
    case lists:keyfind(?WORKER_ID(Name), 1, Workers) of
        {_Id, Child, _Type, _Modules} when is_pid(Child) ->
            {ok, Child};
        _ ->
            {error, not_found}
    end.

child_spec(Id, {M, _F, _A} = Start, Restart, Type) ->
    #{id => ?WORKER_ID(Id),
      start => Start,
      restart => Restart,
      shutdown => ?SHUTDOWN_TIME,
      type => Type,
      modules => [M]}.
