Nonterminals expr parameters.

Terminals '=' '+' '-' '*' '/' '(' ')' ',' ':' '[' ']' '"' number 
          word.

Rootsymbol expr.

Expect 2.

Right   0    '='.
Left    1    '('.
Left    2    ')'.
Left    3    '['.
Left    4    ']'.
Left    5    '"'.
Left    6    '+'.       %% +
Left    10   '-'.       %% -
Left    15   '*'.       %% *
Left    20   '/'.       %% /
Left    25   ','.       %% ,
Left    30   ':'.       %% :
%% Expressions

%% List expressions
expr  ->       '"' word '"'            : extract_string('$2').
expr  ->       '[' parameters ']'      : '$2'.       %% reuse 'parameters from next section

%% Define function calls, with parsing the parameters
expr  ->       word ':' word '(' parameters ')' : call_function('$1', '$3', '$5').
expr  ->       word '(' parameters ')' : call_function('$1', '$3').
parameters ->  expr                    : ['$1'].
parameters ->  expr ',' parameters     : ['$1' | '$3'].

%% handling expressions with the most basic operators
expr  -> '(' expr ')'      : '$2'.
expr  -> number            : get_number('$1').
expr  -> word              : get_var('$1').
expr  -> expr '+' expr     : '$1' + '$3'.
expr  -> expr '-' expr     : '$1' - '$3'.
expr  -> expr '*' expr     : '$1' * '$3'.
expr  -> expr '/' expr     : '$1' / '$3'.
expr  -> word '=' expr     : store_var('$1', '$3').
expr  -> '-'expr           : - '$2'.
expr  -> '+'expr           : + '$2'.

Erlang code.

-export([sqrt/1]).

-define(VARS, calc_vars).

get_number({number, _Line, V}) -> V.

store_var(Key, Value) ->
    Vars = get_vars(),
    erlang:put(?VARS, dict:store(Key, Value, Vars)),
    Value.

get_var(Key) ->
    case dict:find(Key, get_vars()) of
        error -> throw({no_variable, Key});
        {ok, Value} -> Value
    end.

get_vars() ->
    case erlang:get(?VARS) of
        undefined -> dict:new();
        Vars -> Vars
    end.

call_function({word, _Line, ModName}, {word, _Line, FuncName}, Value)  ->
    io:format("Value = ~p~n", [Value]),
    Mod = list_to_atom(ModName),
    Function = list_to_atom(FuncName),
    erlang:apply(Mod, Function, Value).

call_function({word, _Line, Name}, Value)  ->
    Function = list_to_atom(Name),
    erlang:apply(?MODULE, Function, Value).

sqrt(V) -> math:sqrt(V).

extract_string({word, _Line, Chars}) -> Chars.
