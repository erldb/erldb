Definitions.
Comment     = ((/\*([^*]|\*)*\*/)|(//.*\n))
HiddenChars = ((\n)|(\r)|(\t)|(\s)|(\f))
Identifier  = ([a-zA-Z]+[_a-zA-Z0-9]*)

Rules.

\:\:           : {token, {'validator', TokenLine}}.
\:             : {token, {'colon', TokenLine}}.
\(             : {token, {'lparen', TokenLine}}.
\)             : {token, {'rparen', TokenLine}}.
[0-9]+         : {token, {'int_constant', list_to_integer(TokenChars), TokenLine}}.
name           : {token, {'name', TokenLine}}.
backend        : {token, {'backend', TokenLine}}.
fields         : {token, {'fields', TokenLine}}.
functions      : {token, {'functions', TokenLine}}.
integer        : {token, {'integer', TokenLine}}.
string         : {token, {'string', TokenLine}}.
binary         : {token, {'binary', TokenLine}}.
float          : {token, {'float', TokenLine}}.
datetime       : {token, {'datetime', TokenLine}}.
timestamp      : {token, {'timestamp', TokenLine}}.
boolean        : {token, {'boolean', TokenLine}}.
mixed          : {token, {'mixed', TokenLine}}.
{Identifier}   : {token, {'identifier', list_to_atom(TokenChars), TokenLine, TokenLen}}.
{HiddenChars}+ : skip_token.
{Comment}      : skip_token.

Erlang code.
