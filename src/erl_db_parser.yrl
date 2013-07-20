Nonterminals type argument arguments field fields fields_decl attributes attributes_list model.
Terminals atom integer string float '=' ',' '(' ')' ':' '::' '.'.

Rootsymbol model.

'type' -> 'atom' : '$1'.
'type' -> 'integer' : '$1'.
'type' -> 'string' : '$1'.
'type' -> 'float' : '$1'.
'type' -> 'atom' '.' 'atom' : field_ref('$1', '$3').

'argument' -> 'type' : '$1'.
'argument' -> 'atom' '=' 'type' : {'$1', '$3'}.

'arguments' -> '$empty' : [].
'arguments' -> 'argument' ',' 'arguments' : [ '$1' | '$3' ].
'arguments' -> 'argument' : [ '$1' ].

'field' -> 'atom' '::' 'atom' '(' 'arguments' ')' : field('$1', '$3', '$5').
'field' -> 'atom' '::' 'atom' '(' ')' : field('$1', '$3', []).

'fields' -> 'field' : [ '$1' ].
'fields' -> 'field' 'fields' : [ '$1' | '$2' ].

'attributes' -> 'atom' ':' 'type' : attribute('$1', '$3', []).
'attributes' -> 'atom' ':' 'type' '(' 'arguments' ')' : attribute('$1', '$3', '$5').
'attributes' -> 'atom' ':' 'fields' : '$3'.

'attributes_list' -> 'attributes' : [ '$1' ].
'attributes_list' -> 'attributes' 'attributes_list' : [ '$1' | '$2' ].

'fields_decl' -> 'atom' ':' 'fields' : '$3'.

'model' -> 'attributes_list' : '$1'.

%% We don't support inline code right now
%%'model' -> 'functions_decl' : '$1'.


Erlang code.
-include("../include/erl_db_types.hrl").
-export([]).

attribute(Key, Value, Arguments) ->
    #'ATTRIBUTE'{
           key = Key,
           value = Value,
           arguments = Arguments
          }.

field(Identifier, Type, Arguments) ->
    #'FIELD'{
       name = Identifier,
       type = Type,
       arguments = Arguments
      }.

field_ref(Model, Field) ->
    #'FIELD_REF'{
           model = Model,
           field = Field
          }.
