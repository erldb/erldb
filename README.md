# erldb

ORM (Object-relational mapping) application implemented in Erlang.

# Configure erldb


# Writing models

## Basic structure
A model is defined by a basic set of attributes.



## Relations

## Trigger functions


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

# Building the examples

Check the ``examples/``-directory for example models. To compile the model tags.erl;
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
  $ erl -pa ebin/
  Erlang R16B01 (erts-5.10.2) [source-bdf5300] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

  Eshell V5.10.2  (abort with ^G)
  1> rr("include/tags.hrl").
  [tags]
  2> A = #tags{}.
  #tags{id = undefined,title = "Fancy title",text = undefined,
        created = undefined}
  3> A:uppercase_title().
  "FANCY TITLE"
  4>
```
