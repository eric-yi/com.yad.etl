- module(echo).
- export([go/0, loop/0]).

% from "Erlang Programming"
% yi_xiaobin@163.com
% 2015-02-27 Eric Yi

go() ->
    Pid = spawn(echo, loop, []),
    Pid ! {self(), hello},
    receive
        {Pid, Msg} -> io:format("enter: ~w~n", [Msg])
    end,
    Pid ! stop,
    finish.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop -> true
    end.
