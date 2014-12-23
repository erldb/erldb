%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2014, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2014 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(erldb_mysql).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-export([
         create_table/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          connection :: pid()
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
init(Args) ->
    DBHost       = proplists:get_value(db_host,     Args, "localhost"),
    DBPort       = proplists:get_value(db_port,     Args, 3306),
    DBUsername   = proplists:get_value(db_username, Args, "guest"),
    DBPassword   = proplists:get_value(db_password, Args, ""),
    DBDatabase   = proplists:get_value(db_database, Args, "test"),
    {ok, Conn} = mysql_conn:start_link(DBHost, DBPort, DBUsername, DBPassword, DBDatabase,
                                      fun(_, _, _, _) -> ok end, utf8, erldb_pool),
    {ok, #state{
       connection = Conn
      }}.

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
handle_call({init_table, Model, _Args}, _From, State) ->
    Fields = erldb_lib:get_fields(Model),
    Res = create_table(State#state.connection, Model, Fields),
    {reply, Res, State};
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
fetch(Conn, Query) ->
    Res = mysql_conn:fetch(Conn, [Query], self()),
    case Res of
        {error, MysqlRes} = Err ->
            Err;
        _ -> ok
    end,
    Res.

create_table(Conn, TableName, TableDefinition) when is_atom(TableName) ->
    SQL = lists:flatten(["CREATE TABLE IF NOT EXISTS ", erlang:atom_to_list(TableName), " ( ", string:join(table_definition_to_sql_columns(TableDefinition), ","), " )"]),
    fetch(Conn, SQL).

table_definition_to_sql_columns([]) -> [];
table_definition_to_sql_columns([{Fieldname, _Pos, Fieldtype, Args}|Tl]) ->
    Length = case proplists:get_value(length, Args) of
                 Val when is_integer(Val) ->
                     "(" ++ erlang:integer_to_list(Val) ++ ")";
                 _ ->
                     ""
             end,
    [erlang:atom_to_list(Fieldname) ++ " " ++
     type_to_sql(Fieldtype) ++ Length ++ " " ++ options_to_sql(Args)|table_definition_to_sql_columns(Tl)].

options_to_sql([]) -> [];
options_to_sql([{length, _}|Tl]) -> options_to_sql(Tl);
options_to_sql([{default, Value}|Tl]) when is_list(Value) ->
    ["DEFAULT \"" ++ Value ++ "\""|options_to_sql(Tl)];
options_to_sql([{default, Value}|Tl]) ->
    ["DEFAULT " ++ Value|options_to_sql(Tl)];
options_to_sql([not_null|Tl]) ->
    ["NOT NULL"|options_to_sql(Tl)];
options_to_sql([null|Tl]) ->
    ["NULL"|options_to_sql(Tl)];
options_to_sql([auto_increment|Tl]) ->
    ["AUTO_INCREMENT"|options_to_sql(Tl)];
options_to_sql([primary_key|Tl]) ->
    ["PRIMARY KEY"|options_to_sql(Tl)];
options_to_sql([unique|Tl]) ->
    ["UNIQUE KEY"|options_to_sql(Tl)];
options_to_sql([{storage, Type}|Tl]) ->
    Res = case Type of
              disk ->
                  "DISK";
              memory ->
                  "MEMORY";
              default ->
                  "DEFAULT"
          end,
    ["STORAGE " ++ Res|options_to_sql(Tl)].


type_to_sql(ok) ->
    "INTEGER";
type_to_sql(string) ->
    "VARCHAR";
type_to_sql(binary) ->
    "BINARY";
type_to_sql(integer) ->
    "INTEGER";
type_to_sql(float) ->
    "REAL";
type_to_sql(datetime) ->
    "DATETIME";
type_to_sql(timestamp) ->
    "TIMESTAMP";
type_to_sql(boolean) ->
    "ENUM(true,false)".
