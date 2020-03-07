%%%-------------------------------------------------------------------
%%% @author daniel <>
%%% @copyright (C) 2013, daniel
%%% @doc
%%%
%%% @end
%%% Created :  1 Aug 2013 by daniel <>
%%%-------------------------------------------------------------------
-module(erldb_mnesia).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    WorkerArgs = proplists:get_value(worker_args, Args, []),
    gen_server:start_link(?MODULE, WorkerArgs, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    ok = ensure_start(),
    {ok, #state{}}.

handle_call({init_table, Name, Args}, _From, State) ->
    FieldName = get_fieldname(proplists:get_value(module_attr, Args), []),
    Attr = [{disc_copies, [node()]}, {attributes, FieldName} | proplists:delete(module_attr, Args)],
    Result = case mnesia:create_table(Name, Attr) of
		 {atomic, ok} -> {ok, Name};
		 {aborted, {already_exists,tags}} -> {ok, Name};
		 {aborted, Reason} -> {error, Reason}
	     end,
    {reply, Result, State};
handle_call({find, Model, Conditions, _}, _From, State) ->
    [{_,_,Key}] = Conditions,
    Reply = transaction(read, Model, Key),
    {reply, Reply, State};
handle_call({save, Object}, _From, State) ->
    Model = element(1, Object),
    Reply = transaction(write, Model, Object),
    {reply, Reply, State};
handle_call({delete, Object, _Arguments}, _From, State) ->
    Table = element(1, Object),
    Key = element(2, Object),
    Reply = transaction(delete, Table, Key),
    {reply, Reply, State};
handle_call({update, Object, _}, _From, State) ->
    Model = element(1, Object),
    Reply = transaction(write, Model, Object),
    {reply, Reply, State};
handle_call({supported_condition, Conditions}, _From, State) ->
    Supported = ['='],
    List = [Operators || {_, Operators, _} <- Conditions,
			 lists:member(Operators, Supported) == false],
    Reply =
	case List of
	    [] -> {ok, supported};
	    List -> {error, not_supported, List}
	end,
    {reply, Reply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec ensure_start() -> ok | {error, Reaon :: term()}.
ensure_start() ->
    mnesia:create_schema([node()]),
    case application:start(mnesia) of
	ok ->
	    ok;
	{error, {already_started, mnesia}} ->
	    ok;
	Err ->
	    Err
    end.

-spec get_fieldname(list(tuple()), Acc :: list()) -> list().
get_fieldname([], Acc) ->
    lists:reverse(Acc);
get_fieldname([{field, X} | Tail], Acc) ->
    get_fieldname(Tail, [element(1, hd(X)) | Acc]);
get_fieldname([{_,_}|Tail], Acc) ->
    get_fieldname(Tail, Acc).

transaction(Action, Tab, Object) ->
    Fun = case Action of
	      read -> fun() -> mnesia:read(Tab, Object) end;
	      write -> fun() -> mnesia:write(Tab, Object, write) end;
	      delete -> fun() -> mnesia:delete(Tab, Object, write) end
	  end,
    case mnesia:transaction(Fun) of
	{atomic, Response} -> {ok, Response};
        {aborted, Reason} -> {error, Reason}
    end.
