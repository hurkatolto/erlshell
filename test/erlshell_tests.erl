%%------------------------------------------------------------------------------
%% eunit tests erlshell
%%------------------------------------------------------------------------------
-module(erlshell_tests).

-include_lib("eunit/include/eunit.hrl").

cmd_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [fun cmd1_test/0,
      fun shell_default_test/0]}.

setup() ->
    es_utils:store_shell_commands(),
    [].

cleanup(_) ->
    ok.

cmd1_test() ->
    eval_lines(
      1,
      [{"A = 12 +- 1",                   11},
       {"B = 100 - -A",                  111},
       {"L = lists:seq(1,3)",            [1,2,3]},
       {"lists:max(lists:seq(1,3))",     3},
       {"X = Y = -1",                    -1},
       {"X",                             -1},
       {"lists:flatten(io_lib:format(\"~p\", [lists:max([1,3])]))",       "3"},

       %% Logical operators
       {"2 =:= 2",                       true},
       {"2 =:= 2.0",                     false},
       {"2 == 2.0",                      true},
       {"2 =/= 2.0",                     true},
       {"2 < 2.1",                       true},
       {"2 =< 2.0",                      true},
       {"2 > 1.1",                       true},
       {"2 >= 2.0",                      true},
       {"true and true",                 true},
       {"true orelse false",             true}
      ]).

shell_default_test() ->
    es_utils:store_shell_commands(),
    eval_lines(
      1,
      [{"help()",                   true},
       {"i()",                      ok},
       {"uptime()",                 ok}
      ]).

eval_lines(_LC, []) ->
    ok;
eval_lines(LineCount, [CmdAndResult | T]) ->
    eval_line(CmdAndResult, LineCount),
    eval_lines(LineCount + 1, T).

eval_line({Cmd,Expected}, LineCount) ->
    try erlshell:exec_line(Cmd, LineCount) of
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
