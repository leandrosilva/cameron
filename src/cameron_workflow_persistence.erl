%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for requests. It's used to keep track in a database every state (steps?) of a
%%      request that refers to a request to run a workflow. And in this case, it stores those steps
%%      in a Redis server.

-module(cameron_workflow_persistence).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([save_new_request/1, mark_promise_as_dispatched/1]).
-export([save_promise_progress/1, save_error_on_promise_progress/1, mark_promise_as_paid/1]).

%%
%% Includes ---------------------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec save_new_request(Request) -> {ok, Promise} | {error, Reason}
%% @doc The first step of the whole process is accept a request and create a promise which should
%%      be payed, and based on that, one can keep track of workflow execution state and get any
%%      related data at any time in the future.
%%      Status: promised.
save_new_request(#request{workflow = Workflow, key = RequestKey, data = Data, from = From} = Request) ->
  WorkflowName = Workflow#workflow.name,

  NewPromiseUUID = new_promise_uuid(),
  PromiseUUIDTag = redis_promise_tag_for(WorkflowName, RequestKey, NewPromiseUUID),

  ok = redis(["hmset", PromiseUUIDTag,
                       "request.key",     RequestKey,
                       "request.data",    Data,
                       "request.from",    From,
                       "status.current",  "promised",
                       "status.promised", datetime()]),
  
  ok = redis(["set", redis_tag_as_pending(PromiseUUIDTag), PromiseUUIDTag]),
  
  {ok, #promise{uuid = NewPromiseUUID, request = Request}}.

%% @spec mark_promise_as_dispatched(Promise) -> ok | {error, Reason}
%% @doc Status: dispatched, which means it's work in progress.
mark_promise_as_dispatched(#promise{} = Promise) ->
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  ok = redis(["hmset", PromiseUUIDTag,
                       "status.current",         "dispatched",
                       "status.dispatched.time", datetime()]).

%% @spec save_promise_progress(WorkflowStepOutput) -> ok | {error, Reason}
%% @doc Save to Redis a workflow execution output.
%%      Status: paid, for this particular step.
save_promise_progress(#step_output{step_input = StepInput, output = Output}) ->
  Promise = StepInput#step_input.promise,
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  StepName = StepInput#step_input.name,

  ok = redis(["hmset", PromiseUUIDTag,
                       "step." ++ StepName ++ ".status.current",   "paid",
                       "step." ++ StepName ++ ".status.paid.time", datetime(),
                       "step." ++ StepName ++ ".output",           Output]).

%% @spec save_error_on_promise_progress(WorkflowStepOutput) -> ok | {error, Reason}
%% @doc Status: error, this promise is done.
save_error_on_promise_progress(#step_output{step_input = StepInput, output = Output}) ->
  Promise = StepInput#step_input.promise,
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  StepName = StepInput#step_input.name,

  ok = redis(["hmset", PromiseUUIDTag,
                       "step." ++ StepName ++ ".status.current",   "error",
                       "step." ++ StepName ++ ".status.error.time", datetime(),
                       "step." ++ StepName ++ ".output",           Output]),

  ok = redis(["set", redis_error_tag_for(PromiseUUIDTag, StepName), Output]).

%% @spec mark_promise_as_paid(Promise) -> ok | {error, Reason}
%% @doc Status: paid, this promise is done.
mark_promise_as_paid(#promise{} = Promise) ->
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  ok = redis(["hmset", PromiseUUIDTag,
                       "status.current",   "paid",
                       "status.paid.time", datetime()]),

  1 = redis(["del", redis_tag_as_pending(PromiseUUIDTag)]),
  ok = redis(["set", redis_tag_as_paid(PromiseUUIDTag), PromiseUUIDTag]).
  
%%
%% Internal Functions -----------------------------------------------------------------------------
%%

%% redis

redis(Command) ->
  Output = redo:cmd(cameron_redo, Command),
  maybe_ok(maybe_string(Output)).

redis_workflow_tag_for(WorkflowName) ->
  % cameron:workflow:{name}:
  re:replace("cameron:workflow:{name}:", "{name}", maybe_string(WorkflowName), [{return, list}]).

redis_promise_tag_for(WorkflowName, RequestKey, PromiseUUID) ->
  % cameron:workflow:{name}:key:{key}:promise:{uuid}
  redis_workflow_tag_for(WorkflowName) ++ "key:" ++ RequestKey ++ ":promise:" ++ PromiseUUID.
  
redis_promise_tag_for(#promise{uuid = PromiseUUID} = Promise) ->
  Request = Promise#promise.request,
  Workflow = Request#request.workflow,

  WorkflowName = Workflow#workflow.name,
  PromiseUUID = Promise#promise.uuid,
  RequestKey = Request#request.key,
  
  redis_promise_tag_for(WorkflowName, RequestKey, PromiseUUID).

redis_tag_as_pending(AnyTag) ->
  AnyTag ++ ":pending".

redis_tag_as_paid(AnyTag) ->
  AnyTag ++ ":paid".

redis_error_tag_for(PromiseUUIDTag, Step) ->
  PromiseUUIDTag ++ ":" ++ Step ++ ":error".

%% promise

new_promise_uuid() ->
  uuid_helper:new().

%% helpers

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
  