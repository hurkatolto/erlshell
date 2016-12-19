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
expr  ->       string                  : extract_string('$1').
expr  ->       '[' parameters ']'      : '$2'.       %% reuse 'parameters from next section
expr  ->       '{' parameters '}'      : list_to_tuple('$2').       %% reuse 'parameters from next section

%% atom expression
expr  ->       atom                  : extract_atom('$1').

%% Define function calls, with parsing the parameters
expr  ->       word_or_atom ':' word_or_atom '(' parameters ')' : call_function('$1', '$3', '$5').
expr  ->       word_or_atom '(' parameters ')' : call_function('$1', '$3').
parameters ->  '$empty'                : [].
parameters ->  expr                    : ['$1'].
parameters ->  expr ',' parameters     : ['$1' | '$3'].

%% handling expressions with the most basic operators
expr  -> '(' expr ')'           : '$2'.
expr  -> number                 : get_number('$1').
expr  -> word                   : list_to_atom(extract_atom('$1')).
expr  -> var                    : get_var('$1').
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
expr  -> var  '=' expr          : store_var('$1', '$3').
expr  -> '-'expr                : - '$2'.
expr  -> '+'expr                : + '$2'.

%% Logical operators
expr  -> expr 'and' expr        : '$1' and '$3'.
expr  -> expr 'or' expr         : '$1' or '$3'.
expr  -> expr 'andalso' expr    : '$1' andalso '$3'.
expr  -> expr 'orelse' expr     : '$1' orelse '$3'.

word_or_atom ->  word      : list_to_atom(extract_string('$1')).
word_or_atom ->  atom      : extract_string('$1').

Erlang code.

-export([sqrt/1]).

-define(VARS, calc_vars).

get_number({number, _Line, V}) -> V.

store_var(Key, Value) ->
    Vars = get_vars(),
    case dict:find(Key, Vars) of
        error -> 
            erlang:put(?VARS, dict:store(Key, Value, Vars)),
            Value;
        {ok, V} when V =:= Value ->
            %% Variable is already used, but stores the same value
            V;
        {ok, V} ->
            %% Variable is already used, but it has an other value.
            %% Throw an exception
            es_utils:exception("no match of right hand side value ~p", [V])
    end.

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

call_function(Mod, Function, Parameters)  ->
    erlang:apply(Mod, Function, Parameters).

%%
%% Shell command. Try shell_default module first, if the functions is not there,
%% try user_default. If function is not even in that module throw an exception.
%%
call_function(Function, Parameters)  ->
    PLength = length(Parameters),
    case lists:member({Function, PLength}, erlang:get(shell_default_functions)) of
        true ->
            erlang:apply(shell_default, Function, Parameters);
        false ->
            case lists:member({Function, PLength}, erlang:get(user_default_functions)) of
                true ->
                    erlang:apply(user_default, Function, Parameters);
                false ->
                    es_utils:exception("undefined shell command ~p/~p",
                                       [Function, PLength])
            end
    end.

sqrt(V) -> math:sqrt(V).

extract_string({_, _Line, Chars}) -> Chars.

extract_atom({_, _Line, Atom}) -> Atom.
