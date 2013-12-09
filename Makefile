# Project

PROJECT = erldb

# Options

PLT_APPS = crypto public_key ssl

# Dependencies

DEPS = poolboy
dep_poolboy = https://github.com/devinus/poolboy.git 1.0.1

# Targets

include erlang.mk

