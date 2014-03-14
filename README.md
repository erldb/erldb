# erldb

ORM (Object-relational mapping) application implemented in Erlang.

# Usage

Check the ``examples/``-directory for example models. To compile the model tags.erl;
```
  ~/erldb$ erl -pa ebin/
  Erlang R16B01 (erts-5.10.2) [source] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

  Eshell V5.10.2  (abort with ^G)
  1> erldb_compiler:compile("examples/tags.erl").
  {ok,"tags.beam"}
  2> rr("tags.hrl").
  [tags]
  3> A = #tags{}.
  #tags{id = undefined,title = "Fancy title",text = undefined,
      created = undefined}
  4> A:uppercase_title().
  "FANCY TITLE"
```

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
  $ make dialyzer
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