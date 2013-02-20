# Erl_DB
ORM (Object-relational mapping) application implemented in Erlang.

# Model files
Check 'testmodel.model' in the examples/ directory

# Usage
Check the file 'test.erl' inside the src-directory

## Compile

Usage:
----------

        rebar compile model

To compile your code and to compile your models.

        Eshell V5.9.2  (abort with ^G)
        1> application:start(erl_db).
        21:41:44.883 [info] Application lager started on node nonode@nohost
        21:41:44.951 [info] Application mnesia started on node nonode@nohost
        21:41:44.951 [info] Application erl_db started on node nonode@nohost
        ok
        2>