-module(atom2json).
-export([main/1, xml_to_json/2]).
-include_lib("xmerl/include/xmerl.hrl").

% convert all XML files in the Source dir to JSON and place in Dest dir
main([Source, Dest]) ->
  {ok,List} = file:list_dir(Source),
  xml_to_json(List, Source, Dest),
  wait(length(List)).

% convert a list of XML files in the Source dir to JSON and place in Dest dir
xml_to_json([], _Source, _Dest) -> done;
xml_to_json([File|Rest], Source, Dest) ->
  Args = [lists:concat([Source, "/", File]), lists:concat([Dest, "/", File])],
  spawn_link(?MODULE, xml_to_json, Args),
  xml_to_json(Rest, Source, Dest).

% convert Source file from XML to JSON and write to Dest
xml_to_json(Source, Dest) ->
  { Xml, _Rest } = xmerl_scan:file(Source),
  file:write_file(Dest, rfc4627:encode(xml_element(Xml))).

% #xmlElement becomes [binary-name, attributes, [children]]
xml_element(Node) ->
  #xmlElement{name=Name, attributes=Attributes, content=Content} = Node,
  [atom_to_binary(Name), attributes(Attributes,[]), content(Content,[])].

% array of #xmlAttribute becomes {obj, [{name,binary-value}, ...]}
attributes([], List) -> {obj, List};
attributes([Head|Rest], List) ->
  #xmlAttribute{name=Name, value=Value} = Head,
  attributes(Rest, [{Name,list_to_binary(xmerl_ucs:to_utf8(Value))}|List]).

% array of children becomes array of content or binary-text; consecutive
% #xmlText nodes are coalesced; non #xmlElement, non #xmlText are dropped
content([], List) -> lists:reverse(List);
content([Element|Rest], List) when record(Element, xmlElement) ->
  content(Rest, [xml_element(Element)|List]);
content([Text1,Text2|Rest], List) when record(Text1, xmlText), record(Text2, xmlText) ->
  #xmlText{value=Value1} = Text1,
  #xmlText{value=Value2} = Text2,
  content([#xmlText{value=Value1++Value2} | Rest], List);
content([Text|Rest], List) when record(Text, xmlText)->
  #xmlText{value=Value} = Text,
  content(Rest, [list_to_binary(xmerl_ucs:to_utf8(Value))|List]);
content([_Other|Rest], List) -> 
  content(Rest, List).
  
% wait for everybody to report "done"
wait(0) -> init:stop();
wait(N) -> wait(receive {'EXIT', _Pid, _Why} -> N-1 end).

% from mysql_conn.erl; IMHO should be a BIF
atom_to_binary(Val) ->
    <<_:4/binary, Bin/binary>> = term_to_binary(Val),
    Bin.
