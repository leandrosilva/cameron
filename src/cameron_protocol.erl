%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc JSON-based protocol.

-module(cameron_protocol).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([parse_request_payload/1]).

%% @spec parse_request_payload(Payload) -> {Key, Data, Requestor} | {error, Reason}
%% @doc Parses payload string from client request.
parse_request_payload(Payload) ->
  Struct = struct:from_json(Payload),
  
  Key = struct:get_value(<<"key">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, list}),
  Requestor = struct:get_value(<<"requestor">>, Struct, {format, list}),
  
  {Key, Data, Requestor}.
