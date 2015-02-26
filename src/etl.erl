- module(etl).
- export([etl/0]).

% 
% yi_xiaobin@163.com
% 2015-01-30 Eric Yi

etl() ->
    io:format("Start ETL...~n"),
    Ok = verify(),
    if
        Ok ->
            preprocess(),
            extract(),
            transform(),
            load();
        not Ok ->
            io:format("not pass verification, eixt.~n")
    end,
    io:format("Finish ETL...~n").

verify() ->
    io:format("==Verify Run.==~n"),
    Result = false,
    % TODO: verify body
    
    io:format("==Verify End.==~n"),
    Result.

preprocess() ->
    io:format("==Preprocess Run.==~n"),
    % TODO: proprecess body
    
    io:format("==Preprocess End.==~n").

extract() ->
    io:format("==Extract Run.==~n"),
    % TODO: extract body

    io:format("==Extract End.==~n").

transform() ->
    io:format("==Transform Run.==~n"),
    % TODO: transform body

    io:format("==Transform End.==~n").

load() ->
    io:format("==Load Run.==~n"),
    % TODO: load body

    io:format("==Load End.==~n").
