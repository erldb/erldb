# Erl_DB
ORM (Object-relational mapping) application implemented in Erlang.

# Model files
An example of a model:
     name: my_module
     backend: mnesia
     /* This is a test comment */
     fields:
             id
             title :: string(255)


# Usage
Check the file 'test.erl' inside the src-directory