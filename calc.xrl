Definitions.
%% Basic calculator with four operators, variables

INT         = [0-9]+
FLOAT       = [0-9]*\.[0-9]+
OPERATOR    = [\+\-\*/\(\)=:,\[\]\{\}"]

Rules.

%% matching a string properly handling escaped double quotes
"([^"\\]++|\\.)*"     : {token, {string, TokenLine, string:sub_string(TokenChars, 2, length(TokenChars)-1)}}.

%% matching atoms similarly
'([^'\\]++|\\.)*'     : {token, {atom, TokenLine, list_to_atom(string:sub_string(TokenChars, 2, length(TokenChars)-1))}}.

{OPERATOR}            : {token, {list_to_atom(TokenChars), TokenLine}}.
[A-Za-z~]+            : {token, {word, TokenLine, TokenChars}}.
{INT}                 : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT}               : {token, {number, TokenLine, to_float(TokenChars)}}.
[\s\t\r]+             : skip_token.
[\n]+                 : {token, {newline, TokenLine}}.

Erlang code.

to_float([$. | Chars]) -> list_to_float("0." ++ Chars);
to_float(Chars) -> list_to_float(Chars).

