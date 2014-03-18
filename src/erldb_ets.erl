%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <burbas@Niclass-MacBook-Pro-2.local>
%%% @copyright (C) 2013, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2013 by Niclas Axelsson <burbas@Niclass-MacBook-Pro-2.local>
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
handle_call({init_table, Name, Args}, _From, State) ->
    Options = proplists:get_value(options, Args, []),
    Result = ets:new(Name, Options),
    {reply, Result, State};

handle_call({save, Object}, _From, State) ->
    Model = element(1, Object),
    case ets:info(Model) of
        undefined ->
            %% We might need to create the tab first
            create_table(Model),
            true = ets:insert(Model, Object);
        _Info ->
            ets:insert(Model, Object)
    end,
    {reply, ok, State};

handle_call({find, Model, Conditions, _Options}, _From, State) ->
    case ets:info(Model) of
        undefined ->
            {reply, {error, tab_not_found}, State};
        _Info ->
            Fields = proplists:get_value(fields, Model:module_info(attributes)),
            Match = build_match_q(Conditions, Fields),
            Object = ets:match_object(Model, Match),
            {reply, Object, State}
    end;

handle_call({delete, Model, Conditions}, _From, State) ->
    Fields = proplists:get_value(fields, Model:module_info(attributes)),
    Match = build_match_q(Conditions, Fields),
    ObjectList = ets:match_object(Model, Match),
    lists:foreach(
      fun(Object) ->
              true = ets:delete_object(Model, Object)
      end, ObjectList),
    erldb_log:msg(info, "Removed ~p objects of type ~p", [length(ObjectList), Model]),
    {reply, ok, State};

handle_call({supported_condition, Conditions}, _From, State) ->
    Supported = ['='],
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
create_table(Model) ->
    Fields = proplists:get_value(fields, Model:module_info(attributes), []),
    {Fieldname, _, _} = lists:keyfind(primary_key, 2, Fields),
    Pos = get_field_pos(Fieldname, Fields),
    ets:new(Model, [public, named_table, {keypos, Pos}]).


build_match_q(QueryFields, Fields) ->
    InitialQuery = ['_'|lists:map(fun(_) ->
                                           '_'
                                   end, Fields)],
    Res = build_match_q(QueryFields, Fields, InitialQuery),
    erlang:list_to_tuple(Res).

build_match_q([], _, Query) ->
    Query;
build_match_q([{Fieldname, 'equals', Value}|Tl], Fields, Query) ->
    Pos = get_field_pos(Fieldname, Fields),
    UpdatedQuery = replace_list_item(Pos, Value, Query),
    build_match_q(Tl, Fields, UpdatedQuery).


replace_list_item(1, Value, [_Out|Fields]) ->
    [Value|Fields];
replace_list_item(Pos, Value, [Field|Fields]) ->
    [Field|replace_list_item(Pos-1, Value, Fields)].

get_field_pos(Fieldname, Fields) ->
    get_field_pos(Fieldname, Fields, 2).

get_field_pos(_Fieldname, [], _Acc) ->
    {error, not_found};
get_field_pos(_Fieldname, [{_Fieldname, _Type, _Args}|_], Acc) ->
    Acc;
get_field_pos(Fieldname, [_|Tl], Acc) ->
    get_field_pos(Fieldname, Tl, Acc+1).
