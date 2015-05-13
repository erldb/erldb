# Project

PROJECT = erldb

# Options

CT_SUITES = erldb_ets
PLT_APPS = kernel stdlib asn1 crypto public_key ssl

# Dependencies

DEPS = poolboy mysql dh_date
dep_poolboy = https://github.com/devinus/poolboy.git master
dep_mysql = https://github.com/dizzyd/erlang-mysql-driver.git
dep_dh_date = https://github.com/daleharvey/dh_date.git

include erlang.mk

