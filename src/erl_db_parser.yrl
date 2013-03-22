Nonterminals value field_argument field_argument_list field fields_list vsn_option backend_decl import_list model.

Terminals 'validator' 'colon' 'dot' 'lparen' 'rparen' 'equal' 'comma' 'int_constant' 'import' 'name' 'backend' 'fields' 'functions' 'identifier' 'vsn'.

Rootsymbol model.

value ->
    'identifier' : '$1'.
value ->
    'int_constant' : '$1'.

field_argument ->
    'identifier' 'dot' 'identifier' : {model_field, {'$1', '$3'}}.
field_argument ->
    'identifier' : '$1'.
field_argument ->
    'identifier' 'equal' value : {'$1', '$3'}.

field_argument_list ->
    field_argument : ['$1'].
field_argument_list ->
    field_argument 'comma' field_argument_list : ['$1'] ++ '$3'.

field ->
    'identifier' 'validator' 'identifier' 'lparen' field_argument_list 'rparen' : field('$1', '$3', '$5').
field ->
    'identifier' 'validator' 'identifier' 'lparen' 'rparen' : field('$1', '$3', []).

fields_list ->
    field : ['$1'].
fields_list ->
    field fields_list : ['$1'] ++ '$2'.

backend_decl ->
    'backend' 'colon' identifier : backend('$3', nil).
backend_decl ->
    'backend' 'colon' identifier 'lparen' 'rparen' : backend('$3', nil).
backend_decl ->
    'backend' 'colon' identifier 'lparen' field_argument_list 'rparen' : backend('$3', '$5').

import_list ->
    'identifier' : [import('$1')].
import_list ->
    'identifier' 'comma' import_list : [import('$1')]++'$3'.

vsn_option ->
    '$empty' : nil.
vsn_option ->
    'vsn' 'colon' 'int_constant' : vsn('$3').

model ->
    'import' 'colon' import_list 'name' 'colon' 'identifier' vsn_option backend_decl 'fields' 'colon' fields_list : model('$3', '$6', '$7', '$8', '$11', []).
model ->
    'name' 'colon' 'identifier' vsn_option backend_decl 'fields' 'colon' fields_list : model([], '$3', '$4', '$5', '$8', []).


Erlang code.
-include("../include/erl_db_types.hrl").
-export([]).

model(Imports, Name, Vsn, Backend, Fields, Functions) ->
    #'MODEL'{
       imports = Imports,
       version = Vsn,
       name = Name,
       backend = Backend,
       fields = Fields,
       functions = Functions
      }.

vsn(Vsn) ->
    #'VERSION'{
     value = Vsn
    }.

backend(Name, Arguments) ->
    #'BACKEND'{
         name = Name,
         arguments = Arguments
        }.

import(Identifier) ->
    #'IMPORT'{
        model = Identifier
       }.

field(Identifier, Type, Arguments) ->
    #'FIELD'{
       name = Identifier,
       type = Type,
       arguments = Arguments
      }.
