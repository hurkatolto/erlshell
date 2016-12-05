Nonterminals lines expr.
Terminals '+' '-' '*' '/' '=' number word newline.
Rootsymbol lines.

lines -> expr newline    : io:format("%p", ['$1']).
expr  -> expr '+' expr   : '$1' + '$3'.
expr  -> expr '-' expr   : '$1' - '$3'.
expr  -> expr '*' expr   : '$1' * '$3'.
expr  -> expr '/' expr   : '$1' / '$3'.

Erlang code.
