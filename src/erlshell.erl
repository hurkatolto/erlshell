-module(erlshell).

-export([main/1,
         exec_line/2]).

main(_Opts) ->
    es_utils:store_shell_commands(),
    read_lines(1).

read_lines(L) ->
    case io:get_line(integer_to_list(L) ++ "> ") of
        eof -> ok;
        Line ->
            exec_line(Line, L),
            read_lines(L + 1)
    end.

exec_line(Line0, LineCount) ->
    try
        Line = chunk_nl(Line0),
        {ok, Tokens, _} = erlshell_lexer:string(Line),
        io:format("Tokens = ~p\n", [Tokens]),
        {ok, Result} = erlshell_parser:parse(Tokens),
        io:format("~p\n", [Result]),
        es_utils:store_command_result(LineCount, Result),
        {ok, Result}
    catch _:Error ->
        io:format("Error: ~p,\nStack = ~p\n", [Error, erlang:get_stacktrace()])
    end.

chunk_nl(L) ->
    case string:tokens(L, "\n") of
        [Line] -> Line;
        [] -> []
    end.
