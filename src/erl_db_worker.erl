-module(erl_db_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    io:format("init... ~n"),
    {ok, #state{conn=ok}}.

handle_call(info, _From, #state{conn=Conn}=State) ->
    {reply, mnesia:info(), State};
handle_call({create_table, Fields}, _From, #state{conn=Conn}=State) ->
    {reply, io:format("Sql: ~p~n", [Sql]), State};
handle_call(Request, _From, State) ->
    {reply, io:format("Request: ~p~n", [Request]), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
