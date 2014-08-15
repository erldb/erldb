%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Example of a tags-model used in erldb
%%% @end
%%% Created : 15 Aug 2014 by Niclas Axelsson <niclas@burbas.se>
-backend(mnesia, []).
-relation(belongs_to, author).

-field(id, string, [primary_key]).
-field(title, string, [{default, "Fancy title"}]).
-field(text, string, []).
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
    case Object#tags.title of
        undefined ->
            stop;
        _ ->
            {ok, Object}
    end.

_post_insert(Object) ->
    case Object#tags.title of
        "PHP" ->
            stop;
        _ ->
            {ok, Object}
    end.

%% Disable all deletions
_pre_delete(_Object) ->
    stop.

%% Disable all deletions
_post_delete(Object) ->
    stop.
