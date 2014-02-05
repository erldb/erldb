# Project

PROJECT = erldb

# Options

PLT_APPS = crypto public_key ssl

# Dependencies

DEPS = poolboy
dep_poolboy = https://github.com/erldb/poolboy.git master

# Targets

include erlang.mk

