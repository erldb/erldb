# Project

PROJECT = erldb

# Options

CT_SUITES = erldb_mnesia
PLT_APPS = kernel stdlib asn1 crypto public_key ssl

# Dependencies

DEPS = poolboy
dep_poolboy = https://github.com/erldb/poolboy.git master

# Targets

include erlang.mk

