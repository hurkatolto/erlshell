Nonterminals expr parameters word_or_atom.

Terminals '=' '+' '-' '*' '/' '(' ')' ',' ':' '[' ']' '{' '}' '<' '>'
          '=:=' '==' '=/=' '>=' '=<' number 'andalso' 'orelse' 'and' 'or' 
          word string atom var 'f'.

Rootsymbol expr.

Expect 2.

Right   0     '='.
Left    10    '(' ')'.
Left    20    '[' ']'.
Left    30    'orelse' 'or'.
Left    40    'andalso' 'and'.
Left    50    '>' '<' '=<' '>='.
Left    60    '=:=' '==' '=/='.
Left    70    '+' '-'.
Left    80    '*' '/'.
Left    90    ','.
Left    100   ':'.

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
expr -> '(' expr ')'           : '$2'.
expr -> number                 : es_utils:get_number('$1').
expr -> word                   : list_to_atom(es_utils:extract_atom('$1')).
expr -> var                    : es_utils:get_var('$1').
expr -> 'f' '(' var ')'        : es_utils:del_var('$3').
expr -> expr '==' expr         : '$1' == '$3'.
expr -> expr '=:=' expr        : '$1' =:= '$3'.
expr -> expr '=/=' expr        : '$1' =/= '$3'.
expr -> expr '<' expr          : '$1' < '$3'.
expr -> expr '>' expr          : '$1' > '$3'.
expr -> expr '>=' expr         : '$1' >= '$3'.
expr -> expr '=<' expr         : '$1' =< '$3'.
expr -> expr '+' expr          : '$1' + '$3'.
expr -> expr '-' expr          : '$1' - '$3'.
expr -> expr '*' expr          : '$1' * '$3'.
expr -> expr '/' expr          : '$1' / '$3'.
expr -> var  '=' expr          : es_utils:store_var('$1', '$3').
expr -> '-'expr                : - '$2'.
expr -> '+'expr                : + '$2'.

%% Logical operators
expr  -> expr 'and' expr        : '$1' and '$3'.
expr  -> expr 'or' expr         : '$1' or '$3'.
%% TODO: looks like this shortcircuit operator does not work.
%%       even with this case the expression '$3' is evaluated before the andalso
%%       operator is being called. Investigate if there is an option for yecc
%%       to change the order of the evaluation!
expr  -> expr 'andalso' expr    : es_utils:'andalso'('$1', '$3').
expr  -> expr 'orelse' expr     : es_utils:'orelse'('$1', '$3').

word_or_atom ->  word      : list_to_atom(es_utils:extract_string('$1')).
word_or_atom ->  atom      : es_utils:extract_string('$1').

Erlang code.
