Nonterminals expr parameters word_or_atom.

Terminals '=' '+' '-' '*' '/' '(' ')' ',' ':' '[' ']' '{' '}' '<' '>' number 
          'andalso' 'orelse' 'and' 'or' word string atom var.

Rootsymbol expr.

Expect 2.

Right   0    '='.
Left    1    '('.
Left    2    ')'.
Left    3    '['.
Left    4    ']'.
Left    6    '+'.
Left    10   '-'.
Left    15   '*'.
Left    20   '/'.
Left    25   ','.
Left    30   ':'.

%% Expressions

%% List expressions
expr  ->       string                  : es_utils:extract_string('$1').
expr  ->       '[' parameters ']'      : '$2'.       %% reuse 'parameters from next section
expr  ->       '{' parameters '}'      : list_to_tuple('$2').       %% reuse 'parameters from next section

%% atom expression
expr  ->       atom                  : es_utils:extract_atom('$1').

%% Define function calls, with parsing the parameters
expr  ->       word_or_atom ':' word_or_atom '(' parameters ')' : 
    es_utils:call_function('$1', '$3', '$5').
expr  ->       word_or_atom '(' parameters ')' : 
    es_utils:call_function('$1', '$3').
parameters ->  '$empty'                : [].
parameters ->  expr                    : ['$1'].
parameters ->  expr ',' parameters     : ['$1' | '$3'].

%% handling expressions with the most basic operators
expr  -> '(' expr ')'           : '$2'.
expr  -> number                 : es_utils:get_number('$1').
expr  -> word                   : list_to_atom(es_utils:extract_atom('$1')).
expr  -> var                    : es_utils:get_var('$1').
expr  -> expr '=' '=' expr      : '$1' == '$4'.
expr  -> expr '=' ':' '=' expr  : '$1' =:= '$5'.
expr  -> expr '=' '/' '=' expr  : '$1' =/= '$5'.
expr  -> expr '<' expr          : '$1' < '$3'.
expr  -> expr '>' expr          : '$1' > '$3'.
expr  -> expr '>' '=' expr      : '$1' >= '$4'.
expr  -> expr '=' '<' expr      : '$1' =< '$4'.
expr  -> expr '+' expr          : '$1' + '$3'.
expr  -> expr '-' expr          : '$1' - '$3'.
expr  -> expr '*' expr          : '$1' * '$3'.
expr  -> expr '/' expr          : '$1' / '$3'.
expr  -> var  '=' expr          : es_utils:store_var('$1', '$3').
expr  -> '-'expr                : - '$2'.
expr  -> '+'expr                : + '$2'.

%% Logical operators
expr  -> expr 'and' expr        : '$1' and '$3'.
expr  -> expr 'or' expr         : '$1' or '$3'.
expr  -> expr 'andalso' expr    : '$1' andalso '$3'.
expr  -> expr 'orelse' expr     : '$1' orelse '$3'.

word_or_atom ->  word      : list_to_atom(es_utils:extract_string('$1')).
word_or_atom ->  atom      : es_utils:extract_string('$1').

Erlang code.
