Definitions.
%% Basic calculator with four operators, variables

INT         = [0-9]+
FLOAT       = [0-9]*\.[0-9]+
OPERATOR    = ==|=:=|=/=|>=|=<|andalso|and|orelse|or|[\+\-\*/\(\)=:,\[\]\{\}"<>f]

Rules.

%% matching a string properly handling escaped double quotes
"([^"\\]++|\\.)*"     : {token, {string, TokenLine, string:sub_string(TokenChars, 2, length(TokenChars)-1)}}.

%% matching atoms similarly
'([^'\\]++|\\.)*'     : {token, {atom, TokenLine, list_to_atom(string:sub_string(TokenChars, 2, length(TokenChars)-1))}}.

{OPERATOR}            : {token, {list_to_atom(TokenChars), TokenLine}}.
[a-z]+[A-Za-z_0-9]*   : {token, {word, TokenLine, TokenChars}}.
[A-Z]+[A-Za-z_0-9]*   : {token, {var, TokenLine, TokenChars}}.
{INT}                 : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT}               : {token, {number, TokenLine, to_float(TokenChars)}}.
[\s\t\r]+             : skip_token.
[\n]+                 : {token, {newline, TokenLine}}.

Erlang code.

to_float([$. | Chars]) -> list_to_float("0." ++ Chars);
to_float(Chars) -> list_to_float(Chars).
