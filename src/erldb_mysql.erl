%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2014-2015, Niclas Axelsson
%%% @doc
%%% MySQL adapter for ErlDB
%%% @end
%%% Created : 23 Dec 2014 by Niclas Axelsson <niclas@burbas.se>
%%% Updated : 4 Apr 2015 by Niclas Axelsson <niclas@burbas.se>
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
handle_call({find, Model, Conditions, Options}, _From, State) ->
    Query = build_select_query(Model, Conditions, Options),
    Res =
        case fetch(State#state.connection, Query) of
            {data, Data} ->
                MData = mysql:get_result_rows(Data),
                [ list_to_tuple([Model|[ unpack_value(Y) || Y <- X ]]) || X <- MData ];
            Error ->
                {error, Error}
        end,
    {reply, Res, State};
handle_call({Action, Object}, _From, State) when Action == save orelse Action == update ->
    Model = erlang:element(1, Object),
    Zipper = erldb_lib:unzip_object(Object),

    ModelStr = atom_to_binary(Model, utf8),
    SQL = case Action of
              save ->
                  {Fields, Values} =
                      lists:foldl(
                        fun({'id', 'id', _Type, _Args}, Acc) -> Acc;
                           ({'id', Val, Type, Args}, {Attrs, Vals}) -> {["id"|Attrs], [pack_value(Val, Type, Args)|Vals]};
                           ({Field, Val, Type, Args}, {Attrs, Vals}) ->
                                {[atom_to_list(Field)|Attrs], [pack_value(Val, Type, Args)|Vals]}
                        end, {[], []}, Zipper),
                  FieldsStr = list_to_binary(string:join(escape_attr(Fields), ", ")),
                  [
                   <<"INSERT INTO ">>,
                   ModelStr,
                   <<" (">>,
                   FieldsStr,
                   <<") VALUES (">>,
                   Values,
                   <<")">>
                  ];
              update ->
                  SetFields =
                      lists:foldl(
                        fun({'id', _, _, _}, Aux) -> Aux;
                           ({Field, Val, _, _}, Aux) -> [lists:concat(["`", Field, "`=", Val])|Aux]
                        end, [], Zipper),
                  {value, {_, IdVal, _, _}} = lists:keysearch('id', 1, Zipper),
                  [
                   <<"UPDATE " >>,
                   ModelStr,
                   <<"SET ">>,
                   string:join(SetFields, ", "),
                   <<"WHERE id=">>,
                   IdVal
                  ]
          end,
    io:format("~p~n", [SQL]),
    Res = fetch(State#state.connection, SQL),
    {reply, {ok, Res}, State};
handle_call({supported_condition, Conditions}, _From, State) ->
    {reply, {ok, supported}, State};
handle_call({supported_operation, Operation}, _From, State) ->
    Reply = case Operation of
                find_one -> {error, op_not_supported};
                _ -> {ok, supported}
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
fetch(Conn, Query) ->
    case mysql_conn:fetch(Conn, [Query], self()) of
        {error, _MysqlRes} = Err ->
            Err;
        Result ->
            Result
    end.

create_table(Conn, TableName, TableDefinition) when is_atom(TableName) ->
    SQL = lists:flatten(["CREATE TABLE IF NOT EXISTS ", erlang:atom_to_list(TableName), " ( ", string:join(table_definition_to_sql_columns(TableDefinition), ","), " )"]),
    fetch(Conn, SQL).

build_select_query(Type, Conditions, Options) ->
    ["SELECT * FROM ", erlang:atom_to_list(Type),
     case Conditions of
         [] -> "";
         _ ->
             " WHERE " ++ string:join(build_conditions(Conditions), " AND ")
     end,
     " ORDER BY ", atom_to_list(proplists:get_value(order_by, Options, id)), " ", atom_to_list(proplists:get_value(order, Options, desc)),
     case proplists:get_value(limit, Options) of
         undefined -> "";
         {Page, Max} -> [" LIMIT ", integer_to_list(Page), ", ", integer_to_list(Max)];
         Max -> [" LIMIT ", integer_to_list(Max)]
     end].

table_definition_to_sql_columns([]) -> "";
table_definition_to_sql_columns([{Fieldname, _Pos, Fieldtype, Args}|Tl]) ->
    Length = case {Fieldtype, proplists:get_value(length, Args)} of
                 {_, Val} when is_integer(Val) ->
                     "(" ++ erlang:integer_to_list(Val) ++ ")";
                 {string, undefined} ->
                     "(256)";
                 _ ->
                     ""
             end,
    ["`" ++ erlang:atom_to_list(Fieldname) ++ "` " ++
     type_to_sql(Fieldtype) ++ Length ++ " " ++ string:join(options_to_sql(Args), " ")|table_definition_to_sql_columns(Tl)].


escape_attr(Attributes) ->
    [ ["`", Attribute, "`"] || Attribute <- Attributes ].

build_conditions([]) -> [];
build_conditions([{Field, 'equals', Value}|Tl]) ->
    ["`" ++ atom_to_list(Field) ++ "`=\"" ++ Value ++ "\""|build_conditions(Tl)].

options_to_sql([]) -> [];
options_to_sql([{length, _}|Tl]) -> options_to_sql(Tl);
options_to_sql([{default, Value}|Tl]) when is_list(Value) ->
    ["DEFAULT \"", Value, "\""|options_to_sql(Tl)];
options_to_sql([{default, Value}|Tl]) ->
    ["DEFAULT ", pack_value(Value, false, false)|options_to_sql(Tl)];
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
    ["STORAGE ", Res|options_to_sql(Tl)].

unpack_value(<<"FALSE">>) -> false;
unpack_value(<<"TRUE">>) -> true;
unpack_value(Int) when is_integer(Int) ->
    Int;
unpack_value(MaybeTerm) when is_binary(MaybeTerm) ->
    case catch erlang:binary_to_term(MaybeTerm) of
        {'EXIT', _} ->
            binary_to_list(MaybeTerm);
        Res ->
            Res
    end;
unpack_value(undefined) ->
    undefined;
unpack_value(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
unpack_value(_) ->
    throw(error).

pack_value(undefined, _Type, Args) ->
    proplists:get_value(default, Args, "NULL");
pack_value(Term, erlang_term, _Args) ->
    term_to_binary(Term);
pack_value(true, _Type, _Args) ->
    "\"TRUE\"";
pack_value(false, _Type, _Args) ->
    "\"FALSE\"";
pack_value(Value, _Type, _Args) when is_integer(Value) ->
    integer_to_list(Value);
pack_value(Value, _Type, _Args) when is_binary(Value) ->
    binary_to_list(Value);
pack_value(Value, _Type, _Args) when is_float(Value) ->
    float_to_list(Value);
pack_value(Value, _Type, _Args) when is_atom(Value) ->
    atom_to_list(Value);
pack_value({_,_,_} = DT, _Type, _Args) ->
    dh_date:format("'Y-m-d H:i:s'", DT);
pack_value(Value, _Type, _Args) when is_list(Value) ->
    "'" ++ Value ++ "'".

type_to_sql(ok) ->
    "INTEGER";
type_to_sql(string) ->
    "VARCHAR";
type_to_sql(erlang_term) ->
    "BLOB";
type_to_sql(binary) ->
    "BLOB";
type_to_sql(integer) ->
    "INTEGER";
type_to_sql(float) ->
    "REAL";
type_to_sql(datetime) ->
    "DATETIME";
type_to_sql(timestamp) ->
    "TIMESTAMP";
type_to_sql(boolean) ->
    "ENUM(\"TRUE\",\"FALSE\")".
