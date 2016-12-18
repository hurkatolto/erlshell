%%------------------------------------------------------------------------------
%% eunit tests erlshell
%%------------------------------------------------------------------------------
-module(erlshell_tests).

-include_lib("eunit/include/eunit.hrl").

cmd_tests() ->
    {setup,
     fun() -> io:format(user, "halllo\n\n\n\n\n\n", []) end,
     fun cleanup/1,
     [{"cmd1 test", fun cmd1_test/0},
      {"shell default tests", fun shell_default_test/0}]}.

setup() ->
    io:format(user, "~p ~p setup: '~p' ~n\n\n\n\n", [?MODULE, ?LINE, setup]),
    erlshell:store_shell_commands(),
    io:format("~p ~p erlshell: '~p' ~n", [?MODULE, ?LINE, erlshell]),
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
       {"lists:flatten(io_lib:format(\"~p\", [lists:max([1,3])]))",       "3"}]).

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
            io:format(user, "~p ~p Other: '~p' ~n", [?MODULE, ?LINE, {Cmd, Expected, Other}]),
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

