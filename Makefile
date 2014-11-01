# Project

PROJECT = erldb

# Options

CT_SUITES = erldb_ets
PLT_APPS = kernel stdlib asn1 crypto public_key ssl

# Dependencies

DEPS = poolboy
dep_poolboy = https://github.com/devinus/poolboy.git master

include erlang.mk

