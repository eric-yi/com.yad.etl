%
% yi_xiaobin@163.com
% 2015-03-17 Eric Yi

-module(etl_util).
-include("etl.hrl").
-export([group_system_args/1, open_url/2]).

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

open_url(Root, Url) ->
  {Scheme, Url1} = parse_scheme(Url),
  log4erl:debug("url scheme :~s~n", [Scheme]),

  case Scheme == "file" of
    true ->
      log4erl:debug("enter file : ~s~n", [Url1]),
      Path = parse_file_path(Url1),
      log4erl:debug("url path :~s~n", [Path]),
      [H | _] = Path,
      First = list_to_atom(integer_to_list(H)),
      log4erl:debug("first char of path :~s~n", [First]),
      case First == list_to_atom(integer_to_list(hd("/"))) of
        true ->
          log4erl:debug("enter abstract file path.~n"),
          RealPath = Path,
          log4erl:debug("url real path :~s~n", [RealPath]);
        false ->
          RealPath = [Root, "/", Path],
          log4erl:debug("url real path :~s~n", [RealPath])
      end,
      log4erl:debug("url path :~s~n", [Path]);
    false ->
      {Authority, Url2} = parse_authority(Url1),
      log4erl:debug("url authority :~s~n", [Authority]),

      {User, HostPort} = parse_user_info(Authority),
      log4erl:debug("url user :~s~n", [User]),

      {Host, Port} = parse_host_port(HostPort),
      log4erl:debug("url host=~s, port=~s~n", [Host, Port]),

      {Path, Url3} = parse_path(Url2),
      log4erl:debug("url path :~s~n", [Path]),

      {Parameter, Url4} = parse_parameter(Url3),
      log4erl:debug("url parameters :~s~n", [Parameter]),

      Frag = parse_frag(Url4),
      log4erl:debug("url frags :~s~n", [Frag])
  end,

  void.

parse_scheme(Url) ->
  parse_scheme(Url, []).

parse_scheme([$: | Url], Acc) ->
  {lists:reverse(Acc), Url};
parse_scheme([], Acc) ->
  {[], lists:reverse(Acc)};
parse_scheme([C | Rest], Acc) ->
  parse_scheme(Rest, [C | Acc]).

parse_authority("//" ++ Url) ->
  parse_authority(Url, "");
parse_authority(Url) ->
  Url.

parse_authority([$/ | Rest], Acc) ->
  {lists:reverse(Acc), [$/ | Rest]};
parse_authority([], Acc) ->
  {lists:reverse(Acc), []};
parse_authority([C | Rest], Acc) ->
  parse_authority(Rest, [C | Acc]).

parse_user_info(Authority) ->
  parse_user_info(Authority, []).

parse_user_info([$@ | HostPort], Acc) ->
  {lists:reverse(Acc), HostPort};
parse_user_info([], Acc) ->
  {[], lists:reverse(Acc)};
parse_user_info([C | HostPort], Acc) ->
  parse_user_info(HostPort, [C | Acc]).

parse_host_port(HostPort) ->
  case string:tokens(HostPort, ":") of
    [Host] -> {Host, ""};
    [Host, Port] -> {Host, list_to_integer(Port)};
    _ -> throw({url_error, {invalid_host_port, HostPort}})
  end.

parse_path(Url) ->
  parse_path(Url, []).

parse_path([C | Url], Acc) when C == $?; C == $# ->
  {lists:reverse(Acc), [C | Url]};
parse_path([], Acc) ->
  {lists:reverse(Acc), ""};
parse_path([C | Url], Acc) ->
  parse_path(Url, [C | Acc]).

parse_file_path("///" ++ Url)->
  Url.

parse_parameter([$? | Url]) ->
  parse_parameter(Url, []);
parse_parameter(Url) ->
  {"", Url}.

parse_parameter([$# | Url], Acc) ->
  {lists:reverse(Acc), [$# | Url]};
parse_parameter([], Acc) ->
  {lists:reverse(Acc), ""};
parse_parameter([C | Rest], Acc) ->
  parse_parameter(Rest, [C | Acc]).

parse_frag([$# | Frag]) ->
  unquote(Frag);
parse_frag("") ->
  "";
parse_frag(Data) ->
  throw({url_error, {data_left_after_parsing, Data}}).

unquote(Str) ->
  unquote(Str, []).

unquote([], Acc) ->
  lists:reverse(Acc);
unquote([$+ | Str], Acc) ->
  unquote(Str, [$  | Acc]);
unquote([$\%, A, B | Str], Acc) ->
  unquote(Str, [erlang:list_to_integer([A, B], 16) | Acc]);
unquote([C | Str], Acc) ->
  unquote(Str, [C | Acc]).

new(Scheme, User, Host, Port, Path, Parameter, Frag, url) ->
  update_raw(#url{scheme = Scheme,
                  user = unquote(User),
                  host = Host,
                  port = Port,
                  path = unquote(Path),
                  parameter = Parameter,
                  frag = unquote(Frag),
                  raw = url}).

new(Scheme, User, Host, Port, Path, Parameter, Frag) ->
  update_raw(#url{scheme = Scheme,
                  user = unquote(User),
                  host = Host,
                  port = Port,
                  path = unquote(Path),
                  parameter = Parameter,
                  frag = unquote(Frag)}).

update_raw(url) ->
  url#url{raw = iolist_to_string(bitstring_to_list(url))}.

iolist_to_string(Str) ->
  binary_to_list(iolist_to_binary(Str)).



