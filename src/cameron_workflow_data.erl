%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for requests. It's used to keep track in a database every state (tasks?) of a
%%      request that refers to a request to run a workflow. And in this case, it stores those tasks
%%      in a Redis server.

-module(cameron_workflow_data).
-author('Leandro Silva <leandrodoze@gmail.com>').

% admin api
-export([start_link/0, stop/0]).
% public api
-export([save_new_request/1, mark_job_as_dispatched/1]).
-export([save_job_progress/1, save_error_on_job_progress/1, mark_job_as_done/1]).
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

%% @spec save_new_request(Request) -> {ok, Job} | {error, Reason}
%% @doc The first task of the whole process is accept a request and create a job which should
%%      be payed, and based on that, one can keep track of workflow execution state and get any
%%      related data at any time in the future.
%%      Status: jobd.
save_new_request(#request{} = Request) ->
  NewJob = #job{uuid = new_job_uuid(), request = Request},
  
  ok = gen_server:cast(?MODULE, {save_new_job, NewJob}),

  {ok, NewJob}.

%% @spec mark_job_as_dispatched(Job) -> ok | {error, Reason}
%% @doc Status: dispatched, which means it's work in progress.
mark_job_as_dispatched(#job{} = Job) ->
  ok = gen_server:cast(?MODULE, {mark_job_as_dispatched, Job}).

%% @spec save_job_progress(TaskOutput) -> ok | {error, Reason}
%% @doc Save to Redis a workflow execution output.
%%      Status: done, for this particular task.
save_job_progress(#task_output{} = TaskOuput) ->
  ok = gen_server:cast(?MODULE, {save_job_progress, TaskOuput}).

%% @spec save_error_on_job_progress(TaskOutput) -> ok | {error, Reason}
%% @doc Status: error, this job is done.
save_error_on_job_progress(#task_output{} = TaskOuput) ->
  ok = gen_server:cast(?MODULE, {save_job_progress, TaskOuput}).

%% @spec mark_job_as_done(Job) -> ok | {error, Reason}
%% @doc Status: done, this job is done.
mark_job_as_done(#job{} = Job) ->
  ok = gen_server:cast(?MODULE, {mark_job_as_done, Job}).

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

% save a new job
handle_cast({save_new_job, #job{uuid = JobUUID, request = Request}}, State) ->
  #request{workflow = #workflow{name = WorkflowName},
           key      = RequestKey,
           data     = RequestData,
           from     = RequestFrom} = Request,
  
  JobUUIDTag = redis_job_tag_for(WorkflowName, RequestKey, JobUUID),

  ok = redis(["hmset", JobUUIDTag,
                       "request.key",      RequestKey,
                       "request.data",     RequestData,
                       "request.from",     RequestFrom,
                       "status.current",   "scheduled",
                       "status.scheduled", datetime()]),
  
  ok = redis(["set", redis_tag_as_pending(JobUUIDTag), JobUUIDTag]),
  
  {noreply, State};

% mark a new job as dispatched
handle_cast({mark_job_as_dispatched, #job{} = Job}, State) ->
  JobUUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", JobUUIDTag,
                       "status.current",         "dispatched",
                       "status.dispatched.time", datetime()]),

  {noreply, State};

% save the progress of a job given
handle_cast({save_job_progress, #task_output{task_input = TaskInput, output = Output}}, State) ->
  Job = TaskInput#task_input.job,
  JobUUIDTag = redis_job_tag_for(Job),

  TaskName = TaskInput#task_input.name,

  ok = redis(["hmset", JobUUIDTag,
                       "task." ++ TaskName ++ ".status.current",   "done",
                       "task." ++ TaskName ++ ".status.done.time", datetime(),
                       "task." ++ TaskName ++ ".output",           Output]),

  {noreply, State};

% save an error message happened in a job payment process
handle_cast({save_error_on_job_progress, #task_output{task_input = TaskInput, output = Output}}, State) ->
  Job = TaskInput#task_input.job,
  JobUUIDTag = redis_job_tag_for(Job),

  TaskName = TaskInput#task_input.name,

  ok = redis(["hmset", JobUUIDTag,
                       "task." ++ TaskName ++ ".status.current",   "error",
                       "task." ++ TaskName ++ ".status.error.time", datetime(),
                       "task." ++ TaskName ++ ".output",           Output]),

  ok = redis(["set", redis_error_tag_for(JobUUIDTag, TaskName), Output]),

  {noreply, State};

% mark a job as done
handle_cast({mark_job_as_done, #job{} = Job}, State) ->
  JobUUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", JobUUIDTag,
                       "status.current",   "done",
                       "status.done.time", datetime()]),
io:format("~n~n-------------------------~n~n"),
  1 = redis(["del", redis_tag_as_pending(JobUUIDTag)]),
  ok = redis(["set", redis_tag_as_done(JobUUIDTag), JobUUIDTag]),

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

redis_job_tag_for(WorkflowName, RequestKey, JobUUID) ->
  % cameron:workflow:{name}:key:{key}:job:{uuid}
  redis_workflow_tag_for(WorkflowName) ++ "key:" ++ RequestKey ++ ":job:" ++ JobUUID.
  
redis_job_tag_for(#job{uuid = JobUUID} = Job) ->
  Request = Job#job.request,
  Workflow = Request#request.workflow,

  WorkflowName = Workflow#workflow.name,
  JobUUID = Job#job.uuid,
  RequestKey = Request#request.key,
  
  redis_job_tag_for(WorkflowName, RequestKey, JobUUID).

redis_tag_as_pending(AnyTag) ->
  AnyTag ++ ":pending".

redis_tag_as_done(AnyTag) ->
  AnyTag ++ ":done".

redis_error_tag_for(JobUUIDTag, Task) ->
  JobUUIDTag ++ ":" ++ Task ++ ":error".

%% job

new_job_uuid() ->
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
  