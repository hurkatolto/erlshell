Nonterminals lines expr.
Terminals '+' '-' '*' '/' '=' number word newline.
Rootsymbol lines.

lines -> expr newline    : io:format("%p", ['$1']).
expr  -> expr '+' expr   : '$1' + '$3'.
expr  -> expr '-' expr   : '$1' - '$3'.
expr  -> expr '*' expr   : '$1' * '$3'.
expr  -> expr '/' expr   : '$1' / '$3'.

%% list -> '[' ']'       : [].
%% list -> '[' elems ']' : '$2'.

%% elems -> elem           : ['$1'].
%% elems -> elem ',' elems : ['$1'|'$3'].

%% elem -> int  : extract_token('$1').
%% elem -> atom : extract_token('$1').
%% elem -> list : '$1'.

Erlang code.
