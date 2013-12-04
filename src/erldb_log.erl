-module(erldb_log).

-export([msg/2, msg/3]).

msg(Level, Message) ->
    msg(Level, Message, []).

msg(Level, Message, Params) ->
    io:format("################## ~p ###################~n", [Level]), io:format(Message ++ "~n", Params).
