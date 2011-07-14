%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for requests. It's used to keep track in a database every state (steps?) of a
%%      request that refers to a request to run a workflow. And in this case, it stores those steps
%%      in a Redis server.

-module(cameron_workflow).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([lookup/1, accept_new_request/1, take_next_promise/1, save_progress/1, mark_as_paid/1]).

%%
%% Includes ---------------------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec lookup(Name) -> {Name, {start_url, URL}} | undefined
%% @doc Get a workflow configuration by name.
lookup(Name) when is_atom(Name) ->
  Spec = proplists:get_value(Name, cameron:get_workflows_config(), undefined),
  
  case Spec of 
    undefined        -> undefined;
    {start_url, URL} -> #workflow{name = Name, start_url = URL}
  end;
  
lookup(Name) when is_list(Name) ->
  lookup(list_to_atom(Name)).

%% @spec accept_new_request(Request) -> {ok, Promise} | {error, Reason}
%% @doc The first step of the whole process is accept a request and create a promise which should
%%      be payed, and based on that, one can keep track of workflow execution state and get any
%%      related data at any time in the future.
accept_new_request(#request{workflow = Workflow, key = Key, data = Data, from = From} = Request) ->
  io:format("--- [cameron_workflow] save a request // Request: ~w~n", [Request]),
  
  Name = Workflow#workflow.name,

  PromiseUUID = new_promise_uuid(),
  PromiseUUIDTag = redis_promise_tag_for(Name, Key, PromiseUUID),

  PendingQueueName = redis_pending_queue_name_for(Name),
  redis(["lpush", PendingQueueName, PromiseUUIDTag]),
  
  ok = redis(["hmset", PromiseUUIDTag,
                       "request.key",     Key,
                       "request.data",    Data,
                       "request.from",    From,
                       "status.current",  "enqueued",
                       "status.enqueued", datetime()]),
  
  io:format("--- [cameron_workflow] create a promise // Promise: ~w~n", [PromiseUUID]),
  
  {ok, #promise{uuid = PromiseUUID, request = Request}}.

%% @spec take_next_promise(Name) -> {ok, Promise} | {error, Reason}
%% @doc It pop the next pending promise (of a request) from the pending queue at Redis.
take_next_promise(Name) ->
  io:format("--- [cameron_workflow] take next promise to dispatch~n"),

  PendingQueueName = redis_pending_queue_name_for(Name),
  PromiseUUIDTag = redis(["rpop", PendingQueueName]),

  [RequestKey,
   RequestData,
   RequestFrom] = redis(["hmget", PromiseUUIDTag, 
                                  "request.key", "request.data", "request.from"]),

  PromiseUUID = redis_promise_uuid_from(PromiseUUIDTag),
  
  ok = redis(["hmset", PromiseUUIDTag,
                       "step.current",         "dispatched",
                       "step.dispatched.time", datetime()]),

  
  {ok, #promise{uuid = PromiseUUID,
                request = #request{workflow = lookup(Name),
                                   key      = RequestKey,
                                   data     = RequestData,
                                   from     = RequestFrom}}}.

%% @spec save_progress(WorkflowStepOutput) -> {ok, Promise} | {error, Reason}
%% @doc Save to Redis a workflow execution output.
save_progress(#step_output{step_input = StepInput, output = Output}) ->
  io:format("--- [cameron_workflow] saving an output~n"),

  Promise = StepInput#step_input.promise,
  Request = Promise#promise.request,
  Workflow = Request#request.workflow,

  Name = Workflow#workflow.name,
  PromiseUUID = Promise#promise.uuid,
  RequestKey = Request#request.key,
  
  PromiseUUIDTag = redis_promise_tag_for(Name, RequestKey, PromiseUUID),

  ok = redis(["hmset", PromiseUUIDTag,
                       "step." ++ StepInput#step_input.name ++ ".status.current",   "paid",
                       "step." ++ StepInput#step_input.name ++ ".status.paid.time", datetime(),
                       "step." ++ StepInput#step_input.name ++ ".output",           Output]),

  {ok, Promise}.

%% @spec mark_as_paid(Request) -> {ok, Request} | {error, Reason}
%% @doc Save to Redis a step mark_as_paid.
mark_as_paid(#promise{uuid = PromiseUUID} = Promise) ->
  io:format("--- [cameron_workflow] marking a promess as paid~n"),

  Request = Promise#promise.request,
  Workflow = Request#request.workflow,

  Name = Workflow#workflow.name,
  PromiseUUID = Promise#promise.uuid,
  RequestKey = Request#request.key,
  
  PromiseUUIDTag = redis_promise_tag_for(Name, RequestKey, PromiseUUID),

  ok = redis(["hmset", PromiseUUIDTag,
                       "status.current",   "paid",
                       "status.paid.time", datetime()]),

  redis(["lpush", redis_paid_queue_name_for(Name), PromiseUUID]),

  {ok, Promise}.
  
%%
%% Internal Functions -----------------------------------------------------------------------------
%%

redis(Command) ->
  io:format("[redis] Command: ~w~n", [Command]),
  
  Output = redo:cmd(cameron_redo, Command),
  maybe_ok(maybe_string(Output)).

redis_workflow_tag_for(Name) ->
  % cameron:workflow:{name}:
  re:replace("cameron:workflow:{name}:", "{name}", maybe_string(Name), [{return, list}]).

redis_promise_tag_for(Name, Key) ->
  % cameron:workflow:{name}:key:{key}:promise:
  redis_workflow_tag_for(Name) ++ "key:" ++ Key ++ ":promise:".
  
redis_promise_tag_for(Name, Key, UUID) ->
  % cameron:workflow:{name}:key:{key}:promise:{uuid}
  redis_promise_tag_for(Name, Key) ++ UUID.
  
redis_promise_uuid_from(UUIDTag) ->
  % cameron:workflow:{name}:key:{key}:promise:{uuid}
  [_, UUID] = re:split(UUIDTag, ":promise:"),
  maybe_string(UUID).
  
redis_pending_queue_name_for(Name) ->
  % cameron:workflow:{name}:queue:promise:pending
  redis_workflow_tag_for(Name) ++ "queue:promise:pending".

redis_paid_queue_name_for(Name) ->
  % cameron:workflow:{name}:queue:promise:paid
  redis_workflow_tag_for(Name) ++ "queue:promise:paid".

new_promise_uuid() ->
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
  Single;
  
maybe_string(Single) when is_atom(Single) ->
  atom_to_list(Single).

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
  