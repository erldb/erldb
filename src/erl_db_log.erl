-module(erl_db_log).

-export([msg/3]).

%-define(LOG(Level, Message, Params), lager:log(Level, self(), Message, Params)).

-define(LOG(Level, Message, Params), io:format("################## ~p ###################~n", [Level]), io:format(Message ++ "~n", Params)).

msg(Level, Message, Params) ->
    ?LOG(Level, Message, Params).
