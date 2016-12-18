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
    erlshell:store_shell_commands(),
    [].

cleanup(_) ->
    ok.

cmd1_test() ->
    lists:foreach(
      fun eval_line/1,
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
    erlshell:store_shell_commands(),
    lists:foreach(
      fun eval_line/1,
      [{"help()",                   true},
       {"i()",                      ok},
       {"uptime()",                 ok}]).

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
