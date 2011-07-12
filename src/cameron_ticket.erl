%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for tickets. It's used to keep track in a database every state (steps?) of a
%%      ticket that refers to a request to run a workflow. And in this case, it stores those steps
%%      in a Redis server.

-module(cameron_ticket).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([new/1, take_next/1, save_output/1, close/1]).

%%
%% Includes ---------------------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec new(WorkflowRequest) -> {ok, Ticket} | {error, Reason}
%% @doc The first step of the whole process is create a ticket which allow keep track of workflow
%%      execution state and get any related data at any time in the future.
new(#workflow_request{workflow_name = WorkflowName, key = Key, data = Data, from = From} = WorkflowRequest) ->
  io:format("--- [cameron_ticket] create a ticket // WorkflowRequest: ~w~n", [WorkflowRequest]),
  
  UUID = get_new_uuid(),
  
  UUIDTag = redis_uuid_tag_for(UUID),
  TicketTag = redis_ticket_tag_for(WorkflowName, Key, UUID),
  ok = redis(["set", UUIDTag, TicketTag]),

  IncomingQueueName = redis_incoming_queue_name_for(WorkflowName),
  redis(["lpush", IncomingQueueName, UUIDTag]),
  
  ok = redis(["hmset", TicketTag,
                       "request.key",     Key,
                       "request.data",    Data,
                       "request.from",    From,
                       "status.current",  "enqueued",
                       "status.enqueued", datetime()]),
  
  {ok, #workflow_ticket{workflow_name = WorkflowName, key = Key, uuid = UUID}}.

%% @spec take_next(WorkflowName) -> {ok, Ticket} | {error, Reason}
%% @doc It pop the next ticket (of a request for diagnostic) from the incoming queue at Redis.
take_next(WorkflowName) ->
  io:format("--- [cameron_ticket] take next ticket to dispatch~n"),

  IncomingQueueName = redis_incoming_queue_name_for(WorkflowName),
  UUIDTag = redis(["rpop", IncomingQueueName]),
  TicketTag = redis(["get", UUIDTag]),

  UUID = redis_uuid_from(UUIDTag),
  Key = redis(["hget", TicketTag, "request.key"]),
  
  ok = redis(["hmset", TicketTag,
                       "step.current",         "dispatched",
                       "step.dispatched.time", datetime()]),

  {ok, #workflow_ticket{workflow_name = WorkflowName, key = Key, uuid = UUID}}.

%% @spec save_output(WorkflowStepOutput) -> {ok, Ticket} | {error, Reason}
%% @doc Save to Redis a workflow execution output.
save_output(#workflow_step_output{workflow_name = WorkflowName,
                                  ticket_uuid   = UUID,
                                  name          = Name,
                                  url           = _URL,
                                  payload       = _Payload,
                                  output        = Output,
                                  worker_name   = _WorkerName}) ->
                                    
  io:format("--- [cameron_ticket] saving an output~n"),

  UUIDTag = redis_uuid_tag_for(UUID),
  TicketTag = redis(["get", UUIDTag]),
  Key = redis(["hget", TicketTag, "request.key"]),

  ok = redis(["hmset", TicketTag,
                       "step." ++ Name ++ ".status.current",   "done",
                       "step." ++ Name ++ ".status.done.time", datetime(),
                       "step." ++ Name ++ ".output",           Output]),

  {ok, #workflow_ticket{workflow_name = WorkflowName, key = Key, uuid = UUID}}.

%% @spec close(Ticket) -> {ok, Ticket} | {error, Reason}
%% @doc Save to Redis a step close.
close(#workflow_ticket{workflow_name = WorkflowName, key = _Key, uuid = UUID} = Ticket) ->
  io:format("--- [cameron_ticket] close a ticket~n"),

  UUIDTag = redis_uuid_tag_for(UUID),
  TicketTag = redis(["get", UUIDTag]),

  ok = redis(["hmset", TicketTag,
                       "status.current",   "done",
                       "status.done.time", datetime()]),

  redis(["lpush", redis_done_queue_name_for(WorkflowName), UUID]),

  {ok, Ticket}.
  
%%
%% Internal Functions -----------------------------------------------------------------------------
%%

redis(Command) ->
  io:format("[redis] Command: ~w~n", [Command]),
  
  Output = redo:cmd(cameron_redo, Command),
  maybe_ok(maybe_string(Output)).

redis_uuid_tag_for(UUID) ->
  "cameron:uuid:" ++ UUID.

redis_uuid_from(UUID) ->
  re:replace(UUID, "cameron:uuid:", "", [{return, list}]).
  
redis_workflow_tag_for(WorkflowName) ->
  % cameron:workflow:{name}:
  re:replace("cameron:workflow:{name}:", "{name}", WorkflowName, [{return, list}]).

redis_ticket_tag_for(WorkflowName, Key) ->
  % cameron:workflow:{name}:key:{key}:ticket:
  redis_workflow_tag_for(WorkflowName) ++ "key:" ++ Key ++ ":ticket:".
  
redis_ticket_tag_for(Workflow, Key, UUID) ->
  % cameron:workflow:{name}:key:{key}:ticket:{uuid}
  redis_ticket_tag_for(Workflow, Key) ++ UUID.
  
redis_incoming_queue_name_for(WorkflowName) ->
  % cameron:workflow:{name}:queue:incoming
  redis_workflow_tag_for(WorkflowName) ++ "queue:incoming".

redis_done_queue_name_for(WorkflowName) ->
  % cameron:workflow:{name}:queue:done
  redis_workflow_tag_for(WorkflowName) ++ "queue:done".

get_new_uuid() ->
  uuid_helper:new().

maybe_padding(Number) when is_integer(Number) and (Number < 10) ->
  "0" ++ integer_to_list(Number);

maybe_padding(Number) when is_integer(Number) and (Number > 60) ->
  List = integer_to_list(Number),
  maybe_padding(List);
  
maybe_padding(Number) when is_integer(Number) and (Number > 9) and (Number < 61) ->
  integer_to_list(Number);
  
maybe_padding(List) when is_list(List) ->
  case (string:len(List) =/= 4) and (string:len(List) < 6) of
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
  
datetime() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  
  lists:concat([maybe_padding(Month), "-", maybe_padding(Day),    "-", maybe_padding(Year), " ",
                maybe_padding(Hour),  ":", maybe_padding(Minute), ":", maybe_padding(Second)]).
  