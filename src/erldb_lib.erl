-module(erldb_lib).
-export([
         get_fields/1,
         unzip_object/1,
         make_test/0
        ]).

-include("include/tags.hrl").

is_loaded(Model) ->
    case code:ensure_loaded(Model) of
        {module, _} ->
            true;
        {error, _} ->
            false
    end.


get_fields(Model) ->
    [ Y || {Z,[Y]} <- Model:module_info(attributes),
           Z =:= field].


unzip_object(Object) ->
    Model = element(1, Object),
    is_loaded(Model),
    Fields = get_fields(Model),
    Res = lists:map(
            fun({Field, Position, Type, Args}) ->
                    {Field, element(Position, Object), Type, Args}
            end, Fields),
    Res.



make_test() ->
    {ok, Pid} = erldb_mysql:start_link([{worker_args, [{db_username, "root"}, {db_password, "root"}]}]),
    A = #tags{author_id=0, id=1, text="HERRO", created=erlang:now()},
    gen_server:call(Pid, {save, A}).
