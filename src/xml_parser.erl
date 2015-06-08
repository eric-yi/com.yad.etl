%
% yi_xiaobin@163.com
% 2015-06-04 Eric Yi

-module(xml_parser).
-include_lib("xmerl/include/xmerl.hrl").
-export([read_xml/1, process_xml/2]).

read_xml(File) ->
  {Document, _} = xmerl_scan:file(File),
  Document.

process_attributes(#xmlElement{attributes=Attributes}, Rules) ->
  lists:foldl(fun (Attr, Acc) -> process_attribute(Attr, Acc, Rules) end, [], Attributes).

process_attribute(#xmlAttribute{} = Attribute, State, Rules) ->
  Name = Attribute#xmlAttribute.name,
  Value = Attribute#xmlAttribute.value,
  ProcessedValue = process_value(Value, proplists:get_value(Name, Rules)),
  [{Name, ProcessedValue}|State].

process_value(Value, Fun) when is_function(Fun ,1) -> Fun(Value);
process_value(Value, to_atom) -> list_to_atom(Value);
process_value(Value, to_integer) -> list_to_integer(Value);
process_value(Value, to_binary) -> list_to_binary(Value);
process_value(Value, _) -> Value.


element_text_content(#xmlElement{}=Element) ->
  Content = Element#xmlElement.content,
  ContentHead = hd(Content),
  RawValue = ContentHead#xmlText.value,
  iolist_to_binary(RawValue).

process_xml(Element, Rules) -> process_xml(Element, Rules, []).
process_xml(#xmlElement{name=TagName} = Element, {TagName, AttributesRules, ChildrenRules}, State) when is_list(ChildrenRules) ->
  [{ElTag, process_xml(El, Rule, State)} || 
   #xmlElement{name=ElTag} = El <- Element#xmlElement.content,
   {RuleTag, _, _}=Rule <- ChildrenRules,
   RuleTag==ElTag
  ] ++ process_attributes(Element, AttributesRules) ++ State;
process_xml(#xmlElement{} = Element, Rule, _State) -> 
  process_value(element_text_content(Element), Rule);
process_xml(_, _, State) -> State.
