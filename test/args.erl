
- module(args).
- export([run/0, run/1]).

run() ->
    io:format("test0~n").

run(Args) ->
    io:format("test~n"),
    io:format("~s~n", Args).
