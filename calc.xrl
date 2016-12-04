Definitions.
%% Basic calculator with four operators, variables

INT         = [0-9]+
FLOAT       = [0-9]*\.[0-9]+

Rules.

[\n]+                 : {token, {newline, TokenLine}}.
\+                    : {token, {'+', TokenLine}}.
-                     : {token, {'-', TokenLine}}.
\*                    : {token, {'*', TokenLine}}.
/                     : {token, {'/', TokenLine}}.
\(                    : {token, {'(', TokenLine}}.
\)                    : {token, {')', TokenLine}}.
=                     : {token, {'=', TokenLine}}.
:                     : {token, {':', TokenLine}}.
,                     : {token, {',', TokenLine}}.
\[                    : {token, {'[', TokenLine}}.
\]                    : {token, {']', TokenLine}}.
"                     : {token, {'"', TokenLine}}.
[A-Za-z~]+             : {token, {word, TokenLine, TokenChars}}.
{INT}                 : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT}               : {token, {number, TokenLine, to_float(TokenChars)}}.
[\s\t\r]+             : skip_token.

Erlang code.
to_float([$. | Chars]) -> list_to_float("0." ++ Chars);
to_float(Chars) -> list_to_float(Chars).

