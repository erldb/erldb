# erldb

![Erlang CI](https://github.com/erldb/erldb/workflows/Erlang%20CI/badge.svg?branch=master)

ORM (Object-relational mapping) application implemented in Erlang.

# Configure erldb


# Writing models

## Basic structure
A model is defined by a basic set of attributes.



## Relations
There is currently two different relations in erldb;

`-relation(has, AMOUNT, MODEL_NAME).` - Defines a one to AMOUNT relation.
`-relation(belongs_to, MODEL_NAME).`

## Trigger functions


### Pre triggers

### Post triggers

## Compile the models

# Get workers

TBA

# Build
To build erldb use Makefile to build.

```
  $ make
```

make also builds your model that is in your application environment.

## Documentation
To generate documentation of erldb:

```
  $ make docs
```

# Try it out!!

Check the ``models/``-directory for example models. To compile the model tags.erl;
```
  $ make
   APP    poolboy.app.src
   APP    erldb.app.src
  ./priv/compilemodel
  model_path: "./models"
  Start compiling
  "author.erl"... {ok,"author.beam"}
  "tags.erl"... {ok,"tags.beam"}
  Done compiling
```

Now you can read the record definitions for each of the models within erlang:
```
  $ erl -pa ebin/ deps/*/ebin
  Erlang R16B01 (erts-5.10.2) [source-bdf5300] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

  Eshell V5.10.2  (abort with ^G)
  1> rr("include/tags.hrl").
  [tags]
  2> A = #tags{}.
  #tags{id = undefined,title = "Fancy title",text = undefined,
        created = undefined}
  3> B = A:uppercase_title().
  "FANCY TITLE"
  4> B:save()
  {ok,#tags{author_id = undefined,id = id,
          title = "FANCY TITLE",text = undefined,created = undefined}}
  5> erldb:find(tags, []).
  {ok,[#tags{author_id = undefined,id = id,
           title = "FANCY TITLE",text = undefined,
           created = undefined}]}
```
