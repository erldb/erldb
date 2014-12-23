%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2013, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(erldb_ets).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          tabs = []
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    gen_server:start_link(?MODULE, WorkerArgs, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({init_table, Model, Args}, _From, State) ->
    Options = proplists:get_value(worker_options, Args, []),

    Fields = get_fields(Model),
    [PrimaryKeyPos|_] = [ Pos || {_Fieldname, Pos, _Type, Opt} <- Fields,
                                proplists:get_value(primary_key, Opt) /= undefined ],

    Result = ets:new(Model, [named_table, public, {keypos, PrimaryKeyPos}|Options]),
    {reply, {ok, Result}, State};

handle_call({save, Object}, _From, State) ->
    Model = element(1, Object),
    Fields = get_fields(Model),
    [PrimaryKeyPos|_] = [ Pos || {_Fieldname, Pos, _Type, Opt} <- Fields,
                                 proplists:get_value(primary_key, Opt) /= undefined ],

    UpdatedObject =
        case ets:last(Model) of
            '$end_of_table' ->
                erlang:setelement(PrimaryKeyPos, Object, 1);
            Number when is_integer(Number) ->
                erlang:setelement(PrimaryKeyPos, Object, Number+1);
            _ ->
                %% We don't do anything with this since we don't know what kind of scheme we're running at
                Object
        end,

    true = ets:insert_new(Model, UpdatedObject),
    {reply, UpdatedObject, State};

handle_call({update, Object}, _From, State) ->
    Model = element(1, Object),
    Fields = get_fields(Model),
    [PrimaryKeyPos|_] = [ Pos || {_Fieldname, Pos, _Type, Opt} <- Fields,
                                 proplists:get_value(primary_key, Opt) /= undefined ],
    UpdatedObject =
        case ets:last(Model) of
            '$end_of_table' ->
                erlang:setelement(PrimaryKeyPos, Object, 1);
            Number when is_integer(Number) ->
                erlang:setelement(PrimaryKeyPos, Object, Number+1);
            _ ->
                Object
        end,
    true = ets:insert(Model, UpdatedObject),
    {reply, UpdatedObject, State};

handle_call({find, Model, Conditions, _Options}, _From, State) ->
    case ets:info(Model) of
        undefined ->
            {reply, {error, tab_not_found}, State};
        _Info ->
            Fields = get_fields(Model),
            Match = build_match_q(Conditions, Fields),
            Object = ets:match_object(Model, Match),
            {reply, {ok, Object}, State}
    end;

handle_call({delete, Object}, _From, State) ->
    Model = element(1, Object),
    Match = build_match_q_from_object(Object),
    ObjectList = ets:match_object(Model, Match),
    lists:foreach(
      fun(Obj) ->
              true = ets:delete_object(Model, Obj)
      end, ObjectList),
    {reply, ok, State};

handle_call({supported_condition, Conditions}, _From, State) ->
    Supported = ['equals'],
    List = [Operators || {_, Operators, _} <- Conditions,
			 lists:member(Operators, Supported) == false],
    Reply =
	case List of
	    [] -> {ok, supported};
	    List -> {error, not_supported, List}
	end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





build_match_q(QueryFields, Fields) ->
    Query = ['_'|lists:map(fun({Fieldname, _, _, _}) ->
                                   case lists:keyfind(Fieldname, 1, QueryFields) of
                                       false ->
                                           '_';
                                       Match ->
                                           build_col_query(Match)
                                   end
                           end, Fields)],
    erlang:list_to_tuple(Query).


build_col_query({_Fieldname, 'equals', Value}) ->
    Value.

get_fields(Model) ->
    [ Y || {Z,[Y]} <- Model:module_info(attributes),
           Z =:= field ].



build_match_q_from_object(Object) ->
    Model = element(1, Object),
    Fields = get_fields(Model),
    Query = [Model|lists:map(fun({_Fieldname, Pos, _, _}) ->
                                     element(Pos, Object)
                             end, Fields)],
    erlang:list_to_tuple(Query).
