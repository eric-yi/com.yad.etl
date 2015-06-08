-module(test).
-include("test.hrl").
-export([run/0]).

%
% yi_xiaobin@163.com
% 2015-02-26 Eric Yi

test_define() ->
  io:format("define test:~s~n", [?DEF_TEST]).

test_record() ->
  Etl_Xml_Rules = #xml_rules{},
  io:format("record test:~p~n", [Etl_Xml_Rules]).

run() ->
  io:format("==Test Run.==~n"),
  Result = bump([1,2,3,4,5]),
  io:format("result is ~p.~n", [Result]),
  test_define(),
  test_record(),
  io:format("==Test End.==~n").

bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].
