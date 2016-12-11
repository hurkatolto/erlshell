%%------------------------------------------------------------------------------
%% eunit tests erlshell
%%------------------------------------------------------------------------------
-module(erlshell_tests).

-include_lib("eunit/include/eunit.hrl").

cmd_tests() ->
    io:format("~p ~p cmd_tests: '~p' ~n", [?MODULE, ?LINE, cmd_tests]),
    [{"cmd1 test", fun cmd1_test/0}].

cmd1_test() ->
    lists:foreach(
      fun eval_line/1,
      [{"A = 12 +- 1",              12},
       {"B = 100 - -A",             111},
       {"L = lists:seq(1,3)",       [1,2,3]}]).

eval_line({Cmd,Expected}) ->
    try erlshell:exec_line(Cmd) of
        {ok, Expected} -> ok;
        Other ->
            display_error_and_halt(Cmd, Expected, Other)
    catch
        Type:Error ->
            display_error_and_halt(Cmd, Expected, {Type, Error})
    end.

display_error_and_halt(Cmd, Expected, Error) ->
    io:format(user,
              "Diffferent result for command: ~p"
              "Expected = ~p\n"
              "Current result = ~p~n",
              [Cmd, Expected, Error]),
    throw({error, different_result}).


