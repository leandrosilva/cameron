%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for requests. It's used to keep track in a database every state (steps?) of a
%%      request that refers to a request to run a workflow. And in this case, it stores those steps
%%      in a Redis server.

-module(cameron_workflow_persistence).
-author('Leandro Silva <leandrodoze@gmail.com>').

% admin api
-export([start_link/0, stop/0]).
% public api
-export([save_new_request/1, mark_promise_as_dispatched/1]).
-export([save_promise_progress/1, save_error_on_promise_progress/1, mark_promise_as_paid/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes ---------------------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Start cameron server.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec save_new_request(Request) -> {ok, Promise} | {error, Reason}
%% @doc The first step of the whole process is accept a request and create a promise which should
%%      be payed, and based on that, one can keep track of workflow execution state and get any
%%      related data at any time in the future.
%%      Status: promised.
save_new_request(#request{} = Request) ->
  NewPromise = #promise{uuid = new_promise_uuid(), request = Request},
  
  ok = gen_server:cast(?MODULE, {save_new_promise, NewPromise}),

  {ok, NewPromise}.

%% @spec mark_promise_as_dispatched(Promise) -> ok | {error, Reason}
%% @doc Status: dispatched, which means it's work in progress.
mark_promise_as_dispatched(#promise{} = Promise) ->
  ok = gen_server:cast(?MODULE, {mark_promise_as_dispatched, Promise}).

%% @spec save_promise_progress(WorkflowStepOutput) -> ok | {error, Reason}
%% @doc Save to Redis a workflow execution output.
%%      Status: paid, for this particular step.
save_promise_progress(#step_output{} = StepOuput) ->
  ok = gen_server:cast(?MODULE, {save_promise_progress, StepOuput}).

%% @spec save_error_on_promise_progress(WorkflowStepOutput) -> ok | {error, Reason}
%% @doc Status: error, this promise is done.
save_error_on_promise_progress(#step_output{} = StepOuput) ->
  ok = gen_server:cast(?MODULE, {save_promise_progress, StepOuput}).

%% @spec mark_promise_as_paid(Promise) -> ok | {error, Reason}
%% @doc Status: paid, this promise is done.
mark_promise_as_paid(#promise{} = Promise) ->
  ok = gen_server:cast(?MODULE, {mark_promise_as_paid, Promise}).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(_Options) ->
  {ok, #state{}}.

%% @spec handle_call(Request, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% handle_call generic fallback
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

%% @spec handle_cast(Msg, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling cast messages.

% save a new promise
handle_cast({save_new_promise, #promise{uuid = PromiseUUID, request = Request}}, State) ->
  #request{workflow = #workflow{name = WorkflowName},
           key      = RequestKey,
           data     = RequestData,
           from     = RequestFrom} = Request,
  
  PromiseUUIDTag = redis_promise_tag_for(WorkflowName, RequestKey, PromiseUUID),

  ok = redis(["hmset", PromiseUUIDTag,
                       "request.key",     RequestKey,
                       "request.data",    RequestData,
                       "request.from",    RequestFrom,
                       "status.current",  "promised",
                       "status.promised", datetime()]),
  
  ok = redis(["set", redis_tag_as_pending(PromiseUUIDTag), PromiseUUIDTag]),
  
  {noreply, State};

% mark a new promise as dispatched
handle_cast({mark_promise_as_dispatched, #promise{} = Promise}, State) ->
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  ok = redis(["hmset", PromiseUUIDTag,
                       "status.current",         "dispatched",
                       "status.dispatched.time", datetime()]),

  {noreply, State};

% save the progress of a promise given
handle_cast({save_promise_progress, #step_output{step_input = StepInput, output = Output}}, State) ->
  Promise = StepInput#step_input.promise,
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  StepName = StepInput#step_input.name,

  ok = redis(["hmset", PromiseUUIDTag,
                       "step." ++ StepName ++ ".status.current",   "paid",
                       "step." ++ StepName ++ ".status.paid.time", datetime(),
                       "step." ++ StepName ++ ".output",           Output]),

  {noreply, State};

% save an error message happened in a promise payment process
handle_cast({save_error_on_promise_progress, #step_output{step_input = StepInput, output = Output}}, State) ->
  Promise = StepInput#step_input.promise,
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  StepName = StepInput#step_input.name,

  ok = redis(["hmset", PromiseUUIDTag,
                       "step." ++ StepName ++ ".status.current",   "error",
                       "step." ++ StepName ++ ".status.error.time", datetime(),
                       "step." ++ StepName ++ ".output",           Output]),

  ok = redis(["set", redis_error_tag_for(PromiseUUIDTag, StepName), Output]),

  {noreply, State};

% mark a promise as paid
handle_cast({mark_promise_as_paid, #promise{} = Promise}, State) ->
  PromiseUUIDTag = redis_promise_tag_for(Promise),

  ok = redis(["hmset", PromiseUUIDTag,
                       "status.current",   "paid",
                       "status.paid.time", datetime()]),

  1 = redis(["del", redis_tag_as_pending(PromiseUUIDTag)]),
  ok = redis(["set", redis_tag_as_paid(PromiseUUIDTag), PromiseUUIDTag]),

  {noreply, State};

% manual shutdown
handle_cast(stop, State) ->
  {stop, normal, State};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @spec handle_info(Info, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling all non call/cast messages.

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.
terminate(_Reason, _State) ->
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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
  