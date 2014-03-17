%%%-------------------------------------------------------------------
%%% @author Daniel Hallin Widgren <daniel.widgren@gmail.com>
%%% @copyright (C) 2014, Daniel Hallin Widgren
%%% @doc
%%% Erldb ets worker
%%% @end
%%% Created : 24 Feb 2014 by Daniel Hallin Widgren <daniel.widgren@gmail.com>
%%%-------------------------------------------------------------------
-module(erldb_ets).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init_table/2,
	 save/2,
	 find/2,
	 delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    {ok, #state{}}.

handle_call({init_table, Name, Args}, _From, State) ->
    Options = proplists:get_value(options, Args, []),
    Result = ets:new(Name, Options),
    {reply, Result, State};
handle_call({save, Table, Object}, _From, State) ->
    {reply, ets:insert(Table, Object), State};
handle_call({update, Object, _}, _From, State) ->
    Table = element(1, Object),
    {reply, ets:insert(Table, Object), State};
handle_call({find, Table, Key}, _From, State) ->
    {reply, ets:lookup(Table, Key), State};
handle_call({delete, Object, _}, _From, State) ->
    Table = element(1, Object),
    Key = element(2, Object),
    {reply, ets:delete(Table, Key), State}.
    

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% External API
%%%===================================================================

init_table(Name, Args) ->
    gen_server:call(?SERVER, {init_table, Name, Args}).
save(Table, Object) ->
    gen_server:call(?SERVER, {save, Table, Object}).
find(Table, Key) ->
    gen_server:call(?SERVER, {find, Table, Key}).
delete(Table, Key) ->
    gen_server:call(?SERVER, {delete, Table, Key}).
