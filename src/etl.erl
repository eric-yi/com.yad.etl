%
% yi_xiaobin@163.com
% 2015-01-30 Eric Yi

-module(etl).
-include("etl.hrl").
-import(xml_parser, [read_xml/1, process_xml/2]).
-import(etl_util, [group_system_args/1, oepn_erl/2]).
-export([main/1]).

main(Args) ->
  io:format("Main Application Starting~n"),
  Configs = opt_handle(Args),
  {_, Etc} = lists:keyfind('-etc', 1, [Configs]),
  init(atom_to_list(Etc)),
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

init(EtcDir) ->
  init_log(EtcDir),
  init_config(EtcDir),
  void.

init_log(EtcDir) ->
  application:start(log4erl),
  LogConf = [EtcDir, "/log.conf"],
  log4erl:conf(LogConf),
  log4erl:add_logger(messages_log),
  log4erl:debug("Log4erl startup").

init_config(EtcDir) ->
  EtlXml = lists:concat([EtcDir, '/etl.xml']),
  EtlDoc = xml_parser:read_xml(EtlXml),
  XmlRules = #xml_rules{},
  EtlXmlRules = XmlRules#xml_rules.etl,
  log4erl:debug("etl xml rules :~p~n", [EtlXmlRules]),
  Contents = xml_parser:process_xml(EtlDoc, EtlXmlRules),
  log4erl:debug("etl xml contents :~p~n", [Contents]),
  [{datasource, [{_,DatasourceUrl}]}] = Contents,
  log4erl:debug("datasource id :~p~n", [DatasourceUrl]),

  DatasourceContent = etl_util:open_url(EtcDir, DatasourceUrl),
  log4erl:debug("datasource content :~s~n", [DatasourceContent]),
  void.

