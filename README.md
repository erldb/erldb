# erldb

ORM (Object-relational mapping) application implemented in Erlang.

# Usage

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

# Get workers

TBA

# Build
To build erldb use Makefile to build.

```
  $ make
```

make also builds your model that is in your application environment.

## Make documentation
To generate documentation of erldb:

```
  $ make docs
```

## Make and run dialyzer

### Build plt
```
  $ make build-plt
```

### Run dialyzer
```
  $ make dialyze
```

## Build and run tests

### Build tests
```
  $ make build-tests
```

### Run tests
```
  $ make tests
```
NOTE: The tests does not work 100% yet.
