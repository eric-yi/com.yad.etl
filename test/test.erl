- module(test).
- export([run/0]).

% 
% yi_xiaobin@163.com
% 2015-02-26 Eric Yi

run() ->
    io:format("==Test Run.==~n"),
    Result = bump([1,2,3,4,5]),
    io:format("result is ~p.~n", [Result]),
    io:format("==Test End.==~n").

bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].
