% 
% yi_xiaobin@163.com
% 2015-01-30 Eric Yi


- module(etl_starter).
- import(etl_verify, [verify/0]).
- import(etl_preprocess, [preprocess/0]).
- import(etl_extract, [extract/0]).
- import(etl_transform, [transform/0]).
- import(etl_load, [load/0]).
- export([start/0]).

start() ->
    io:format("Start ETL...~n"),
    Ok = etl_verify:verify(),
    if
        Ok ->
            etl_preprocess:preprocess(),
            etl_extract:extract(),
            etl_transform:transform(),
            etl_load:load();
        not Ok ->
            io:format("not pass verification, eixt.~n")
    end,
    io:format("Finish ETL...~n").
