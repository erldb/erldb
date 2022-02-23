# erldb

ORM (Object-relational mapping) application implemented in Erlang.

# Creating models

*erldb* works as a pre-compiler and transforms the models into modules that erlang can understand. The model itself is
expressed as a simple erlang-module with additional attributes.

A basic model could look like this:

```erlang
-module(user).
-field(username, string, #{length => 5}).
-field(password, string, #{length => 5}).
```

## Datatypes

## Options

## Difference in backends
