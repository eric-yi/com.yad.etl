- module(benchmark).
- export([start/1, start_proc/2]).

% from "Erlang Programming"
% yi_xiaobin@163.com
% 2015-02-27 Eric Yi

start(Num) ->
    start_proc(Num, self()).

start_proc(0, Pid) ->
    Pid ! ok;

start_proc(Num, Pid) ->
    NPid = spawn(benchmark, start_proc, [Num-1, Pid]),
    NPid ! ok,
    receive
        ok -> ok
    end.
