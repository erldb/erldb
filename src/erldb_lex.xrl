Definitions.
HiddenChars = ((\n)|(\r)|(\t)|(\s)|(\f))
Identifier  = ([a-zA-Z]+[_a-zA-Z0-9]*)
String      = \".*\"
EscapedIdentifier = ('[a-zA-Z]+[_a-zA-Z0-9]*')
Float = ([0-9]+\.)+[0-9]+

Rules.

\:\:           : {token, {'validator', TokenLine}}.
\:             : {token, {'colon', TokenLine}}.
\(             : {token, {'lparen', TokenLine}}.
\)             : {token, {'rparen', TokenLine}}.
\=             : {token, {'equal', TokenLine}}.
\,             : {token, {'comma', TokenLine}}.
\.             : {token, {'dot', TokenLine}}.
[0-9]+         : {token, {'int_constant', list_to_integer(TokenChars), TokenLine}}.
{Float}        : {token, {'float', TokenChars, TokenLine}}.
import         : {token, {'import', TokenLine}}.
name           : {token, {'name', TokenLine}}.
backend        : {token, {'backend', TokenLine}}.
fields         : {token, {'fields', TokenLine}}.
functions      : {token, {'functions', TokenLine}}.
vsn            : {token, {'vsn', TokenLine}}.
{String}       : {token, {'string', TokenChars, TokenLine}}.
{Identifier}   : {token, {'identifier', list_to_atom(TokenChars), TokenLine, TokenLen}}.
{EscapedIdentifier} : {token, {'identifier', list_to_atom(remove_escapes(TokenChars)), TokenLine, TokenLen}}.
{HiddenChars}+ : skip_token.

Erlang code.

remove_escapes([]) ->
    [];
remove_escapes([$'|Tl]) ->
    remove_escapes(Tl);
remove_escapes([Hd|Tl]) ->
    [Hd|remove_escapes(Tl)].
