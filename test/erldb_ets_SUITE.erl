%%%-------------------------------------------------------------------
%%% @author Daneil Hallin Widgren <daniel.widgren@gmail.com>
%%% @copyright (C) 2014, daniel
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2014 by daniel <>
%%%-------------------------------------------------------------------
-module(erldb_ets_SUITE).

%% init
-export([init_per_suite/1,
	 init_per_group/2,
	 init_per_testcase/2]).

%% end
-export([end_per_suite/1,
	 end_per_group/2,
	 end_per_testcase/2]).

-export([suite/0,
	 groups/0,
	 all/0]).

%% tests
-export([new/0,
	 new/1,
	 save/0,
	 save/1,
	 find/0,
	 find/1,
	 delete/0,
	 delete/1]).

-include_lib("common_test/include/ct.hrl").
-include("../include/tags.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {ok, Cwd} = file:get_cwd(),
    Path = string:tokens(Cwd, "/"),
    NewCwd = fun(P) -> [_,_|Rest] = lists:reverse(P),
		       "/" ++ filename:join(lists:reverse(Rest))
	     end,
    ok = file:set_cwd(NewCwd(Path)),
    application:start(erldb),
    ok = file:set_cwd(Cwd),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(erldb),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [new,save,find,delete].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
new() -> 
    [].
save() -> 
    [].
find() -> 
    [].
delete() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
new(_Config) -> 
    Tags = #tags{id=1, title="test"},
    1 =:= Tags#tags.id.

save(_Config) -> 
    Tags = #tags{id=1, title="test"},
    {ok, Tags} = erldb:save(Tags).
find(_Config) -> 
    Tags = #tags{id=1, title="test"},
    {ok, Tags} = erldb:find(tags, [{id, '=', 1}]).
delete(_Config) -> 
    {ok, Tag} = erldb:find(tags, [{id, '=', 1}]),
    {ok, _} = erldb:delete(Tag).
