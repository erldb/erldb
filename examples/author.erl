%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Example of a author-model used in erldb
%%% @end
%%% Created : 15 Aug 2014 by Niclas Axelsson <niclas@burbas.se>
-compile({parse_transform, erldb_compiler}).

-table(#{name => "my_table",
         columns => [
                     {id, integer, [auto_increment]},
                     {username, string, [{length, 10}]},
                     {password, string, [{length, 10}]}
                    ]
        }).
