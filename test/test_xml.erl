- module(test_xml).
- include_lib("xmerl/include/xmerl.hrl"). 
- export([run/0]).

% 
% yi_xiaobin@163.com
% 2015-03-17 Eric Yi

run() ->
    io:format("==Test Run.==~n"),
    {Result,Misc} = parse("/data/etl/etc/etl.xml"),
    io:format(Result),
    io:format("==Test End.==~n").

parse(File) ->
    xmerl_scan:file(File).

