Nonterminals value field_argument field_argument_list field fields_list backend_decl import_list model.

Terminals 'validator' 'colon' 'lparen' 'rparen' 'equal' 'comma' 'int_constant' 'import' 'name' 'backend' 'fields' 'functions' 'identifier'.

Rootsymbol model.

value ->
    'identifier' : extract('$1').
value ->
    'int_constant' : extract('$1').

field_argument ->
    'identifier' : extract('$1').
field_argument ->
    'identifier' 'equal' value : {extract('$1'), '$3'}.

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
    'backend' 'colon' identifier : '$3'.

import_list ->
    'identifier' : [extract_val_line('$1')].
import_list ->
    'identifier' 'comma' import_list : [extract_val_line('$1')]++'$3'.

model ->
    'import' 'colon' import_list 'name' 'colon' 'identifier' backend_decl 'fields' 'colon' fields_list : model('$3', '$6', '$7', '$10', []).
model ->
    'name' 'colon' 'identifier' backend_decl 'fields' 'colon' fields_list : model([], '$3', '$4', '$7', []).


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

extract_val_line(Value = {_,_}) ->
    Value;
extract_val_line({int_constant, Value, TokenLine}) ->
    {Value, TokenLine};
extract_val_line({'identifier', Value, TokenLine, _TokenLen}) ->
    {Value, TokenLine}.


model(Imports, Name, Backend, Fields, Functions) ->
    #'MODEL'{
       imports = Imports,
       name = extract_val_line(Name),
       backend = extract_val_line(Backend),
       fields = Fields,
       functions = Functions
      }.

field({'identifier', Fieldname, Line, _Len}, Type, Arguments) ->
    #'FIELD'{
       name = Fieldname,
       type = extract(Type),
       arguments = Arguments,
       line = Line
      }.
