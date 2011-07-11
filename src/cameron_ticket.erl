%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for tickets. It's used to keep track in a database every state (steps?) of a
%%      ticket that refers to a request to run a workflow. And in this case, it stores those steps
%%      in a Redis server.

-module(cameron_ticket).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([create_new/1, take_next/1, save_output/1, close/1, uuid/0]).

%%
%% Includes ---------------------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec create_new(WorkflowRequest) -> {ok, Ticket} | {error, Reason}
%% @doc The first step of the whole process is create a ticket which allow keep track of workflow
%%      execution state and get any related data at any time in the future.
create_new(#workflow_request{workflow_name = WorkflowName, key = Key, data = Data, from = From} = WorkflowRequest) ->
  io:format("--- [cameron_ticket] create a ticket // WorkflowRequest: ~w~n", [WorkflowRequest]),
  
  {ok, Ticket} = get_new_ticket(WorkflowName, Key),

  redis(["lpush", get_incoming_queue_name(WorkflowName), Ticket#workflow_ticket.uuid]),
  ok = redis(["hmset", Ticket#workflow_ticket.uuid,
                       "request.key",      Key,
                       "request.data",     Data,
                       "request.from",     From,
                       "status.current",   "enqueued",
                       "request.key",      Key,
                       "request.enqueued", datetime_as_string()]),
  
  {ok, to_short_ticket_uuid(Ticket)}.

%% @spec take_next(WorkflowName) -> {ok, Ticket} | {error, Reason}
%% @doc It pop the next ticket (of a request for diagnostic) from the incoming queue at Redis.
take_next(WorkflowName) ->
  io:format("--- [cameron_ticket] take next ticket to dispatch~n"),

  Ticket = redis(["rpop", get_incoming_queue_name(WorkflowName)]),
  0 = redis(["hset", Ticket#workflow_ticket.uuid,
                     "step.current",         "dispatched",
                     "step.dispatched.time", datetime_as_string()]),

  ShortUUID = to_short_ticket_uuid(Ticket),

  {ok, ShortUUID}.

%% @spec save_output(WorkflowStepOutput) -> {ok, Ticket} | {error, Reason}
%% @doc Save to Redis a workflow execution output.
save_output(#workflow_step_output{workflow_name = WorkflowName,
                                  ticket_short_uuid = ShortUUID,
                                  name = Name,
                                  output = Output}) ->
                                    
  io:format("--- [cameron_ticket] save a output~n"),

  LongUUID = to_long_ticket_uuid(WorkflowName, ShortUUID),

  1 = redis(["hset", LongUUID,
                     "step." ++ Name ++ ".status",    "done",
                     "step." ++ Name ++ ".done.time", datetime_as_string(),
                     "step." ++ Name ++ ".output",    Output]),

  {ok, ShortUUID}.

%% @spec close(Ticket) -> {ok, Ticket} | {error, Reason}
%% @doc Save to Redis a step close.
close(#workflow_ticket{workflow_name = WorkflowName} = Ticket) ->
  io:format("--- [cameron_ticket] close a ticket~n"),

  LongUUID = to_long_ticket_uuid(Ticket),

  0 = redis(["hset", LongUUID,
                     "status.current",   "done",
                     "status.done.time", datetime_as_string()]),

  redis(["lpush", get_done_queue_name(WorkflowName), LongUUID]),

  {ok, Ticket}.

%% @spec uuid() -> Integer as String
%% @doc Get a incr value from Redis.
uuid() ->
  integer_to_list(redis(["incr", "cameron:uuid"])).
  
%%
%% Internal Functions -----------------------------------------------------------------------------
%%

redis(Command) ->
  Output = redo:cmd(cameron_redo, Command),
  maybe_ok(maybe_string(Output)).

redis_root_tag_for(WorkflowName) ->
  re:replace("cameron:workflow:{name}:", "{name}", WorkflowName, [{return, list}]).

redis_ticket_tag_for(WorkflowName) ->
  redis_root_tag_for(WorkflowName) ++ "ticket:".
  
get_incoming_queue_name(WorkflowName) ->
  redis_root_tag_for(WorkflowName) ++ "queue:incoming".

get_done_queue_name(WorkflowName) ->
  redis_root_tag_for(WorkflowName) ++ "queue:done".

get_new_ticket(WorkflowName, Key) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  {_MegaSecs, _Secs, MicroSecs} = now(),
  
  Tag = redis_ticket_tag_for(WorkflowName),
  Timestamp = lists:concat([maybe_padding(Year), maybe_padding(Month), maybe_padding(Day),
                            maybe_padding(Hour), maybe_padding(Minute), maybe_padding(Second),
                            maybe_padding(MicroSecs)]),
                            
  ShortUUID = Key ++ ":" ++ Timestamp,
  UUID = Tag ++ ShortUUID,
  
  {ok, #workflow_ticket{workflow_name  = WorkflowName,
                        uuid           = UUID,
                        short_uuid     = ShortUUID,
                        tag_part       = Tag,
                        key_part       = Key,
                        timestamp_part = Timestamp}}.

to_short_ticket_uuid(Ticket) ->
  Ticket#workflow_ticket.key_part ++ ":" ++ Ticket#workflow_ticket.timestamp_part.
  
to_long_ticket_uuid(WorkflowName, ShortUUID) ->
  Tag = redis_ticket_tag_for(WorkflowName),
  _UUID = Tag ++ ShortUUID.

to_long_ticket_uuid(Ticket) ->
  Tag = redis_ticket_tag_for(Ticket#workflow_ticket.workflow_name),
  ShortUUID = Ticket#workflow_ticket.short_uuid,
  
  _UUID = Tag ++ ShortUUID.
  
maybe_padding(Number) when is_integer(Number) and (Number < 10) ->
  "0" ++ integer_to_list(Number);

maybe_padding(Number) when is_integer(Number) and (Number > 60) ->
  List = integer_to_list(Number),
  maybe_padding(List);
  
maybe_padding(Number) when is_integer(Number) and (Number > 9) and (Number < 61) ->
  integer_to_list(Number);
  
maybe_padding(List) when is_list(List) ->
  case string:len(List) < 6 of
    true ->
      NewList = "0" ++ List,
      maybe_padding(NewList);
    false ->
      List
  end.

maybe_string([]) ->
  [];
  
maybe_string([Binary | _Tail] = BinaryList) when is_binary(Binary) ->
  maybe_string_(BinaryList, []);
  
maybe_string(Single) when is_binary(Single) ->
  binary_to_list(Single);
  
maybe_string(Single) when is_integer(Single) ->
  Single.

maybe_string_([], StringList) ->
  lists:reverse(StringList);

maybe_string_([Binary | Tail], StringList) when is_binary(Binary) ->
  String = binary_to_list(Binary),
  maybe_string_(Tail, [String | StringList]).

maybe_ok("OK") ->
  ok;
  
maybe_ok(Other) ->
  Other.
  
datetime_as_string() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  
  lists:concat([maybe_padding(Month), "-", maybe_padding(Day),    "-", maybe_padding(Year), " ",
                maybe_padding(Hour),  ":", maybe_padding(Minute), ":", maybe_padding(Second)]).
  