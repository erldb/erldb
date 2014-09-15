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
handle_call({init_table, Name, Args}, _From, State) ->
    Options = proplists:get_value(worker_options, Args, []),
    Result = ets:new(Name, [named_table|Options]),
    {reply, {ok, Result}, State};

handle_call({save, Object}, _From, State) ->
    Model = element(1, Object),
    case ets:info(Model) of
        undefined ->
            %% We might need to create the tab first
            create_table(Model),
            true = ets:insert(Model, Object);
        _Info ->
            true = ets:insert(Model, Object)
    end,
    {reply, ok, State};

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

handle_call({delete, Model, Conditions}, _From, State) ->
    Fields = get_fields(element(1, Model)),
    Match = build_match_q(Conditions, Fields),
    ObjectList = ets:match_object(Model, Match),
    lists:foreach(
      fun(Object) ->
              true = ets:delete_object(Model, Object)
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
create_table(Model) ->
    Fields = proplists:get_value(fields, Model:module_info(attributes), []),
    {Fieldname, _, _} = lists:keyfind(primary_key, 2, Fields),
    Pos = get_field_pos(Fieldname, Fields),
    ets:new(Model, [public, named_table, {keypos, Pos}]).




build_match_q(QueryFields, Fields) ->
    Query = ['_'|lists:map(fun(Fieldname) ->
                                   case lists:keyfind(Fieldname, QueryFields) of
                                       false ->
                                           '_';
                                       Match ->
                                           build_col_query(Match)
                                   end
                           end, Fields)],
    erlang:list_to_tuple(Query).


build_col_query({Fieldname, 'equals', Value}) ->
    {Fieldname, Value}.


get_field_pos(_Fieldname, []) ->
    {error, not_found};
get_field_pos(Fieldname, [{Fieldname, Pos, _Type, _Args}|_]) ->
    Pos;
get_field_pos(Fieldname, [_|Tl]) ->
    get_field_pos(Fieldname, Tl).
get_fields(Model) ->
    [ X || X = {Z,_Y} <- Model:module_info(attributes),
           Z =:= field ].
