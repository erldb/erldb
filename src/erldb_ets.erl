%%%-------------------------------------------------------------------
%%% @author daniel <>
%%% @copyright (C) 2014, daniel
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2014 by daniel <>
%%%-------------------------------------------------------------------
-module(erldb_ets).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init_table/1,
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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({init_table, Args}, _From, State) ->
    Name = proplists:get_value(name, Args),
    Options = proplists:get_value(options, Args, []),
    Result = ets:new(Name, Options),
    {reply, Result, State};
handle_call({save, Table, Object}, _From, State) ->
    {reply, ets:insert(Table, Object), State};
handle_call({find, Table, Key}, _From, State) ->
    {reply, ets:lookup(Table, Key), State};
handle_call({delete, Table, Key}, _From, State) ->
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

init_table(Args) ->
    gen_server:call(?SERVER, {init_table, Args}).
save(Table, Object) ->
    gen_server:call(?SERVER, {save, Table, Object}).
find(Table, Key) ->
    gen_server:call(?SERVER, {find, Table, Key}).
delete(Table, Key) ->
    gen_server:call(?SERVER, {delete, Table, Key}).
