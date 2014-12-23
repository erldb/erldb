# Project

PROJECT = erldb

# Options

CT_SUITES = erldb_ets
PLT_APPS = kernel stdlib asn1 crypto public_key ssl

# Dependencies

DEPS = poolboy erlang-mysql-driver
dep_poolboy = https://github.com/devinus/poolboy.git master
dep_erlang-mysql-driver = https://github.com/dizzyd/erlang-mysql-driver.git

# Targets
test: all build-tests tests

include erlang.mk

