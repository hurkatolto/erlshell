-module(erlshell).

-export([main/1,
         exec_line/3,
         get_commands/1]).

main(_Opts) ->
    es_utils:store_shell_commands(),
    read_lines(1, []).

read_lines(L, Continuation) ->
    case io:get_line(integer_to_list(L) ++ "> ") of
        eof -> ok;
        Line ->
            {NewContinuation, _Res} = exec_line(Line, L, Continuation),
            read_lines(L + 1, NewContinuation)
    end.

exec_line(Line, LineCount, Continuation) ->
    {Cmds, NewContinuation} = get_commands(Continuation ++ Line),
    Res =
        try
            Result = lists:foldl(fun(Cmd, _Acc) ->
                            exec_cmd(Cmd)
                        end, ok, Cmds),
            es_utils:store_command_result(LineCount, Result),
            Result
        catch _:Error ->
            R = lists:flatten(io_lib:format(
                                     "Error: ~p,\nStack = ~p\n",
                                     [Error, erlang:get_stacktrace()])),
            io:format("~s", [R]),
            R
        end,
    {NewContinuation, Res}.

exec_cmd(Cmd) ->
    {ok, Tokens, _} = erlshell_lexer:string(Cmd),
    io:format("~p ~p {Cmd, Tokens}: '~p' ~n", [?MODULE, ?LINE, {Cmd, Tokens}]),
    {ok, Result} = erlshell_parser:parse(Tokens),
    io:format("~p\n", [Result]),
    Result.

get_commands(Str) ->
    get_commands(Str, false, false, [], []).

get_commands([], _InDoubleQutoes, _InQuotes, Cmds, Cont) ->
    {lists:reverse(Cmds), lists:reverse(Cont)};
get_commands([$$, $" | T], false, false, Cmds, Cont) ->
    get_commands(T, false, false, Cmds, [$", $$ | Cont]);
get_commands([$$, $' | T], false, false, Cmds, Cont) ->
    get_commands(T, false, false, Cmds, [$', $$ | Cont]);
get_commands([$" | T], false, false, Cmds, Cont) ->
    get_commands(T, true, false, Cmds, [$" | Cont]);
get_commands([$' | T], false, false, Cmds, Cont) ->
    get_commands(T, false, true, Cmds, [$' | Cont]);
get_commands([$" | T], true, false, Cmds, Cont) ->
    get_commands(T, false, false, Cmds, [$" | Cont]);
get_commands([$' | T], false, true, Cmds, Cont) ->
    get_commands(T, false, false, Cmds, [$' | Cont]);
get_commands([$\\, $" | T], true, false, Cmds, [$", $\\ | Cont]) ->
    get_commands(T, true, false, Cmds, [$", $\\ | Cont]);
get_commands([$\\, $' | T], false, true, Cmds, [$', $\\ | Cont]) ->
    get_commands(T, false, true, Cmds, [$', $\\ | Cont]);
get_commands([C1, $., C2 | T], false, false, Cmds, Cont)
          when C1 >= $0, C1 =< $9, C2 >= $0, C2 =< $9 ->
    get_commands(T, false, false, Cmds, [C2, $., C1 | Cont]);
get_commands([$. | T], false, false, Cmds, Cont) ->
    get_commands(T, false, false, [lists:reverse(Cont) | Cmds], []);
get_commands([C | T], InDoubleQuotes, InQuotes, Cmds, Cont) ->
    get_commands(T, InDoubleQuotes, InQuotes, Cmds, [C | Cont]).
