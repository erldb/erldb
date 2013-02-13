Nonterminals type validation field_declaration field_declaration_list function_declaration_list name_declaration backend_declaration fields_declaration functions_declaration model.

Terminals 'validator' 'colon' 'lparen' 'rparen' 'int_constant' 'name' 'backend' 'fields' 'functions' 'integer' 'string' 'binary' 'float' 'datetime' 'timestamp' 'boolean' 'mixed' 'identifier'.

Rootsymbol model.


type ->
    'integer' : extract('$1').
type ->
    'string' : extract('$1').
type ->
    'binary' : extract('$1').
type ->
    'float'  : extract('$1').
type ->
    'datetime' : extract('$1').
type ->
    'timestamp' : extract('$1').
type ->
    'boolean' : extract('$1').
type ->
    'mixed' : extract('$1').

validation ->
    type 'lparen' 'int_constant' 'rparen' : {'$1', extract('$3')}.
validation ->
    type 'lparen' 'rparen' : {'$1', nil}.

field_declaration ->
    'identifier' : field(line('$1'), '$1', []).
field_declaration ->
    'identifier' 'validator' validation : field(line('$1'), '$1', '$3').

field_declaration_list ->
    field_declaration : ['$1'].
field_declaration_list ->
    field_declaration_list field_declaration : '$1'++['$2'].

function_declaration_list ->
    'identifier' : '$1'.

name_declaration ->
    'name' 'colon' 'identifier' : extract('$3').
backend_declaration ->
    'backend' 'colon' 'identifier' : extract('$3').
fields_declaration ->
    'fields' 'colon' field_declaration_list : '$3'.
functions_declaration ->
    'functions' 'colon' function_declaration_list : '$3'.

model ->
    name_declaration backend_declaration fields_declaration : model('$1', '$2', '$3', []).


Erlang code.
-include("../include/erl_db_types.hrl").
-export([]).


line({_Type, Line}) ->
    Line;
line({_Type, _Str, Line, _Len}) ->
    Line;
line(_) ->
    0.

extract({Value, _TokenLine}) ->
    Value;
extract({int_constant, Value, _TokenLine}) ->
    Value;
extract({'identifier', Ident, _TokenLine, _TokenLen}) ->
    Ident.


model(Name, Backend, Fields, Functions) ->
    #'MODEL'{
       name = Name,
       backend = Backend,
       fields = Fields,
       functions = Functions
      }.

field(Line, Fieldname, Validator) ->
    #'FIELD'{
       name = Fieldname,
       validator = Validator,
       line = Line
      }.
