# erldb

[![Erlang CI](https://github.com/erldb/erldb/workflows/Erlang%20CI/badge.svg?branch=master)](https://github.com/erldb/erldb/actions)

ORM (Object-relational mapping) application implemented in Erlang. Still in very much beta :-)

# Configure erldb

Currently erldb supports two different backends; ets and mysql. We are hoping to support more.

# Writing models
This section will be written soon

## Basic structure
A model is defined by a basic set of attributes.



## Relations

There is currently two different relations in erldb;

`-relation(has, AMOUNT, MODEL_NAME).` - Defines a *one -> N* relation.
`-relation(belongs_to, MODEL_NAME).`

## Trigger functions


### Pre triggers

Pre-triggers can be defined by including the function `_pre_TRIGGERNAME/1` where the *triggername* can be one of: *update*, *lookup*, *delete* or *insert*.

### Post triggers

Post-triggers are defined almost the same as for pre-triggers. Just change *_pre_* to *_post*. Example: `_post_update/1`

## Compile the models

Right now you have to compile the models yourself. Please take a look at the *compilemodel*-script located in the *priv*-directory of this repository.

# Build
To build erldb use rebar3:

```
  $ rebar3 compile
```

## Documentation
To generate documentation of erldb:

```
  $ rebar3 edocs
```

# Try it out!!

Check the ``models/``-directory for example models. To compile the model tags.erl;
```
  $ rebar3 shell

  ...compilation output...

  1> erldb_compiler:compile("examples/tags.erl", [{includedir, "./include"}, {outdir, "."}]).
  {ok,tags}
```

Now you can read the record definitions for each of the models within erlang:
```
  2> rr("include/tags.hrl").
  [tags]
  3> A = #tags{}.
  #tags{id = undefined,title = "Fancy title",text = undefined,
        created = undefined}
  4> B = A:uppercase_title().
  "FANCY TITLE"
  5> B:save()
  {ok,#tags{author_id = undefined,id = id,
          title = "FANCY TITLE",text = undefined,created = undefined}}
  6> erldb:find(tags, []).
  {ok,[#tags{author_id = undefined,id = id,
           title = "FANCY TITLE",text = undefined,
           created = undefined}]}
```
