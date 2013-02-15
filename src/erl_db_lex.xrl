Definitions.
Comment     = ((/\*([^*]|\*)*\*/)|(//.*\n))
HiddenChars = ((\n)|(\r)|(\t)|(\s)|(\f))
Identifier  = ([a-zA-Z]+[_a-zA-Z0-9]*)

Rules.

\:\:           : {token, {'validator', TokenLine}}.
\:             : {token, {'colon', TokenLine}}.
\(             : {token, {'lparen', TokenLine}}.
\)             : {token, {'rparen', TokenLine}}.
\=             : {token, {'equal', TokenLine}}.
\,             : {token, {'comma', TokenLine}}.
[0-9]+         : {token, {'int_constant', list_to_integer(TokenChars), TokenLine}}.
import         : {token, {'import', TokenLine}}.
name           : {token, {'name', TokenLine}}.
backend        : {token, {'backend', TokenLine}}.
fields         : {token, {'fields', TokenLine}}.
functions      : {token, {'functions', TokenLine}}.
{Identifier}   : {token, {'identifier', list_to_atom(TokenChars), TokenLine, TokenLen}}.
{HiddenChars}+ : skip_token.
{Comment}      : skip_token.

Erlang code.
