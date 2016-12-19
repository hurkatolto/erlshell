-module(es_utils).

-export([exception/2,
         store_shell_commands/0,
         store_command_result/2,
         get_command_result/1]).

exception(Fmt, Args) ->
    throw(lists:flatten(io_lib:format(Fmt, Args))).

store_shell_commands() ->
    erlang:put(shell_default_functions, get_module_functions(shell_default)),
    erlang:put(user_default_functions, get_module_functions(user_default)).

store_command_result(LC, Result) ->
    CmdResults = get_cmd_results(),
    erlang:put(cmd_results, dict:store(LC, Result, CmdResults)).

get_command_result(LC) ->
    CmdResults = get_cmd_results(),
    case dict:find(LC, CmdResults) of
        error -> es_utils:exception("~p: command not found", [LC]);
        {ok, Result} ->
            io:format("~p ~p Result: '~p' ~n", [?MODULE, ?LINE, Result]),
            Result
    end.

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
