% 
% yi_xiaobin@163.com
% 2015-01-30 Eric Yi


-module(etl).
-import(etl_util, [parse_xml/1, group_system_args/1]).
-export([main/1]).

main(Args) ->
    io:format("Main Application Starting~n"),
    Configs = opt_handle(Args), 
    {_, Etc} = lists:keyfind('-etc', 1, [Configs]),
    etc_handle(Etc),
    io:format("Main Application Running~n").

opt_handle(Args) ->
    TrimFun = fun(E) -> E == 32 orelse E == 39 end,
    ArgsTrim = lists:reverse(
        lists:dropwhile(
            TrimFun,
            lists:reverse(lists:dropwhile(TrimFun, atom_to_list(hd(Args))), [])
        ),
        []
    ),
    Configs = etl_util:group_system_args(ArgsTrim),
    Configs.

etc_handle(EtcDir) ->
    EtlXml = lists:concat([EtcDir, '/etl.xml']),
    io:format("~s~n", [EtlXml]), 
    etl_util:parse_xml(EtlXml).

