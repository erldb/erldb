%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @doc
%%% Example of a author-model used in erldb
%%% @end
%%% Created : 15 Aug 2014 by Niclas Axelsson <niclas@burbas.se>
-backend(mnesia, []).
-relation(has, many, tags).

-field(id, integer, [primary_key, auto_increment]).
-field(name, string, [{default, "John Doe"}]).
-field(callsign, string).
