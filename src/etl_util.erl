%
% yi_xiaobin@163.com
% 2015-03-17 Eric Yi

-module(etl_util).
-include_lib("xmerl/include/xmerl.hrl"). 
-include("etl.hrl").
-export([read_xml/1, group_system_args/1]).

read_xml(File) ->
    {Document, _} = xmerl_scan:file(File),
    DFile = xmerl_xpath:string("//datasource", Document),
    log4erl:debug("~w~n", [length(DFile)]),
    Document.

group_system_args(Args) ->
    Len = length(Args),
    ArgsIndex = lists:zip(Args, lists:seq(1, Len)),
    group_system_args(Args, ArgsIndex, 1, [], Len).

group_system_args(Args, ArgsIndex, Start, Configs, Limit) ->
    if
        Start > 0, Start < Limit ->
            {Index, Config} = map_system_args(Args, ArgsIndex, Start),
            if
                Index /= -1 -> group_system_args(Args, ArgsIndex, Index, Configs++Config, Limit);
                Index == -1 -> Configs
            end;
        Start =< 0; Start >= Limit -> Configs
    end.

map_system_args(Args, ArgsIndex, Start) when Start > 0 ->
    {Kindex, Key} = subatom(Args, ArgsIndex, 45, 32, Start),
    {Vindex, Value} = get_value(Args, ArgsIndex, Kindex),
    {Vindex, {Key, Value}}.

subatom(List, Map, StartChar, EndChar, StartIndex) when StartIndex > 0 ->
    Start = get_index(Map, StartChar, StartIndex),
    subatom(List, Map, EndChar, Start).

subatom(List, Map, EndChar, Start) when Start > 0 ->
    End = get_index(Map, EndChar, Start),
    if
        End > 0 -> {End, list_to_atom(lists:sublist(List, Start, End-1))};
        End =< 0 -> {-1, notfound}
    end.

get_value(List, Map, Start) when Start > 0 ->
    ScanStart = search_noexsit(Map, Start) + 1, 
    {Index, Value} = subatom(List, Map, 32, ScanStart),
    Len = length(List),
    if
        Value /= notfound -> {Index, Value};
        Value == notfound, ScanStart < Len -> {Len, list_to_atom(lists:sublist(List, ScanStart, Len))};
        Value == notfound, ScanStart =< Len -> {-1, notfound}
    end.

search_noexsit(Map, Start) when Start > 0 ->
    End = get_index(Map, 32, Start),
    if
        End > 0 -> search_noexsit(Map, End);
        End == -1 -> Start
    end.

get_index(Map, Value, Start) ->
    case lists:keyfind(Value, Start, Map) of
        {Value, Index} -> Index;
        false -> -1
    end.

