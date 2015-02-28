% 
% yi_xiaobin@163.com
% 2015-02-28 Eric Yi


- module(etl_verify).
- export([verify/0]).

verify() ->
    io:format("==Verify Run.==~n"),
    Result = false,
    % TODO: verify body
    
    io:format("==Verify End.==~n"),
    Result.
