%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Example of a tags-model used in erldb
%%% @end
%%% Created : 15 Aug 2014 by Niclas Axelsson <niclas@burbas.se>
-backend(ets, []).
-relation(belongs_to, author).

-field(id, string, [primary_key, {length, 128}]).
-field(title, string, [{default, "Fancy title"}, {length, 128}]).
-field(text, string, [{length, 128}]).
-field(created, datetime, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
uppercase_title() ->
    ic_util:to_uppercase(Title).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HOOKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cancel update if text is undefined
_pre_update(Object) ->
    case Object#tags.text of
        undefined ->
            stop;
        _ ->
            {ok, Object}
    end.

%% Logs all updates
_post_update(NewObject, UpdateRes) ->
    {ok, FP} = file:open("update.log", [append]),
    file:write(FP, NewObject),
    file:write(FP, "\n"),
    file:write(FP, UpdateRes),
    file:write(FP, "\n=============\n"),
    file:close(FP).

%% Inserts a new object into the database if the title /= undefined
_pre_insert(Object) ->
    io:format("pre_insert~n"),
    case Object#tags.title of
        undefined ->
            stop;
        _ ->
            io:format("OK on pre_insert~n"),
            {ok, Object}
    end.

%% If the title was equal to "PHP" we abort the insertion.
_post_insert(Object) ->
    io:format("Post insert~n"),
    case Object#tags.title of
        "PHP" ->
            stop;
        _ ->
            io:format("OK on post_insert~n"),
            {ok, Object}
    end.

%% Disable all deletions
_pre_delete(_Object) ->
    stop.

%% Disable all deletions. We don't really need this function since the pre-hook already
%% disabled deletions.
_post_delete(Object) ->
    stop.
