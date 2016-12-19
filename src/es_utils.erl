%%%=============================================================================
%%% @copyright (C) Hurka Tolto
%%% @author Terry Buhl <hurkatolto@gmail.com>
%%% @doc
%%% Various functions for parsing && command execution.
%%% @end
%%%=============================================================================
-module(es_utils).

-export([exception/2,
         store_shell_commands/0,
         store_command_result/2,
         get_command_result/1,
         get_number/1,
         store_var/2,
         get_var/1,
         call_function/3,
         call_function/2,
         extract_string/1,
         extract_atom/1,
         del_var/1,
         del_vars/0,
         'andalso'/2,
         'orelse'/2]).

-define(VARS, calc_vars).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec get_number({number, integer(), term()}) -> term().
get_number({number, _Line, V}) -> V.

-spec exception(string(), list(term())) -> any().
exception(Fmt, Args) ->
    throw(lists:flatten(io_lib:format(Fmt, Args))).

-spec store_var(term(), term()) -> any().
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

-spec get_var(term()) -> any().
get_var(Key) ->
    case dict:find(Key, get_vars()) of
        error -> throw({no_variable, Key});
        {ok, Value} -> Value
    end.

-spec del_var(term()) -> any().
del_var(Key) ->
    Vars = get_vars(),
    case dict:find(Key, Vars) of
        error ->
            throw({no_variable, Key});
        {ok, _Value} ->
            erlang:put(?VARS, dict:erase(Key, Vars)),
            ok
    end.

-spec del_vars() -> ok.
del_vars() ->
    erlang:put(?VARS, dict:new()),
    ok.

-spec call_function(module(), atom(), list(term())) -> any().
call_function(Mod, Function, Parameters)  ->
    erlang:apply(Mod, Function, Parameters).

%%
%% Shell command. Try shell_default module first, if the functions is not there,
%% try user_default. If function is not even in that module throw an exception.
%%
-spec call_function(atom(), list(term())) -> any().
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

-spec extract_string({any(), integer(), string()}) -> string().
extract_string({_, _Line, Chars}) -> Chars.

-spec extract_atom({any(), integer(), atom()}) -> any().
extract_atom({_, _Line, Atom}) -> Atom.

-spec store_shell_commands() -> ok.
store_shell_commands() ->
    erlang:put(shell_default_functions, get_module_functions(shell_default)),
    erlang:put(user_default_functions, get_module_functions(user_default)),
    ok.

-spec store_command_result(integer(), term()) -> ok.
store_command_result(LC, Result) ->
    CmdResults = get_cmd_results(),
    erlang:put(cmd_results, dict:store(LC, Result, CmdResults)),
    ok.

-spec get_command_result(integer()) -> any().
get_command_result(LC) ->
    CmdResults = get_cmd_results(),
    case dict:find(LC, CmdResults) of
        error -> es_utils:exception("~p: command not found", [LC]);
        {ok, Result} ->
            io:format("~p ~p Result: '~p' ~n", [?MODULE, ?LINE, Result]),
            Result
    end.

-spec 'andalso'(boolean(), boolean()) -> any().
'andalso'(A, B) ->
    case A of
        true -> B;
        false -> false
    end.

-spec 'orelse'(boolean(), boolean()) -> any().
'orelse'(A, B) ->
    case A of
        true -> true;
        false -> B
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
get_cmd_results() ->
    case erlang:get(cmd_results) of
        undefined -> dict:new();
        Results -> Results
    end.

get_module_functions(Module) ->
    case catch Module:module_info(exports) of
        {'EXIT', _} -> [];
        Functions -> Functions
    end.

get_vars() ->
    case erlang:get(?VARS) of
        undefined -> dict:new();
        Vars -> Vars
    end.
