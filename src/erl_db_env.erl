-module(erl_db_env).
-export([get_env/3]).


get_env(Application, Key, Default) ->
    case code:is_loaded(boss_env) of
        false ->
            case application:get_env(Application, Key) of
                {ok, Value} ->
                    Value;
                _ ->
                    Default
            end;
        _ ->
            boss_env:get_env(Key, Default)
    end.
