%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction of job's data. So every action on job's data is done thru this module.

-module(cameron_job_data).
-author('Leandro Silva <leandrodoze@gmail.com>').

% admin api
-export([start_link/0, stop/0]).
% public api
-export([create_new_job/2, mark_job_as_running/1, mark_job_as_done/1]).
-export([mark_task_as_running/1, save_task_output/1, save_error_on_task_execution/1]).
-export([get_job_data/3]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes ---------------------------------------------------------------------------------------
%%

-include("cameron.hrl").

-record(state, {}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts cameron_job_data as a gen_server.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec create_new_job(Process, {Key, Input, Requestor}) -> {ok, Job} | {error, Reason}
%% @doc Creates a new job record, which represents an instance of a process.
%%      Status: scheduled.
create_new_job(Process, {Key, Data, Requestor}) ->
  Input = #job_input{key       = Key,
                     data      = Data,
                     requestor = Requestor},
                                      
  NewJob = #job{uuid    = new_uuid(),
                process = Process,
                input   = Input},
  
  ok = gen_server:cast(?MODULE, {create_new_job, NewJob}),

  {ok, NewJob}.

%% @spec mark_job_as_running(Job) -> ok | {error, Reason}
%% @doc Marks a job as running, which means it's work in progress.
%%      Status: running.
mark_job_as_running(#job{} = Job) ->
  ok = gen_server:cast(?MODULE, {mark_job_as_running, Job}).

%% @spec mark_task_as_running(Task) -> ok | {error, Reason}
%% @doc Marks a task as running.
%%      Status: running.
mark_task_as_running(#task{} = Task) ->
  ok = gen_server:cast(?MODULE, {mark_task_as_running, Task}).

%% @spec save_task_output(Task) -> ok | {error, Reason}
%% @doc Saves a task execution output.
%%      Status: done, only for this particular task.
save_task_output(#task{} = Task) ->
  ok = gen_server:cast(?MODULE, {save_task_output, Task}).

%% @spec save_error_on_task_execution(Task) -> ok | {error, Reason}
%% @doc Saves a task execution output error.
%%      Status: error, but the job can still running and becomes done.
save_error_on_task_execution(#task{} = Task) ->
  ok = gen_server:cast(?MODULE, {save_error_on_task_execution, Task}).

%% @spec mark_job_as_done(Job) -> ok | {error, Reason}
%% @doc Marks a job execution as done.
%%      Status: done.
mark_job_as_done(#job{} = Job) ->
  ok = gen_server:cast(?MODULE, {mark_job_as_done, Job}).

%% @spec get_job_data(Job, Key, UUID) -> {ok, Data} | {error, Reason}
%% @doc Gets all available output data from job execution
get_job_data(Job, Key, UUID) ->
  case extract_job_data(Job, Key, UUID) of
    []   -> undefined;
    Data -> {ok, Data}
  end.

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
handle_cast({create_new_job, #job{} = NewJob}, State) ->
  #job_input{key       = Key,
             data      = Data,
             requestor = Requestor} = NewJob#job.input,
    
  UUIDTag = redis_job_tag_for(NewJob),

  ok = redis(["hmset", UUIDTag,
                       "job.key",                   Key,
                       "job.data",                  Data,
                       "job.requestor",             Requestor,
                       "job.status.current",        "scheduled",
                       "job.status.scheduled.time", eh_datetime:now()]),
  
  % now is running
  ok = redis(["set", redis_pending_tag_for(UUIDTag), UUIDTag]),
  
  {noreply, State};

% mark a new job as running
handle_cast({mark_job_as_running, #job{} = Job}, State) ->
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "job.status.current",      "running",
                       "job.status.running.time", eh_datetime:now()]),

  % now it is no longer pending
  1 = redis(["del", redis_pending_tag_for(UUIDTag)]),
  
  % it is running
  ok = redis(["set", redis_running_tag_for(UUIDTag), UUIDTag]),

  {noreply, State};

% mark a task as running
handle_cast({mark_task_as_running, #task{} = Task}, State) ->
  #task{context_job = Job,
        input       = #task_input{requestor = Requestor},
        activity    = #activity_definition{name = Name}} = Task,
  
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "task." ++ Name ++ ".requestor",           Requestor,
                       "task." ++ Name ++ ".status.current",      "running",
                       "task." ++ Name ++ ".status.running.time", eh_datetime:now()]),

  case redis(["hget", UUIDTag, "job.tasks"]) of
    undefined ->
      redis(["hset", UUIDTag, "job.tasks", Name]);
    Tasks ->
      redis(["hset", UUIDTag, "job.tasks", Tasks ++ "," ++ Name])
  end,

  % now it is running
  ok = redis(["set", redis_running_tag_for(UUIDTag, Name), "yes"]),

  {noreply, State};

% save the result of a task/activity given
handle_cast({save_task_output, #task{} = Task}, State) ->
  #task{context_job = Job,
        activity    = #activity_definition{name = Name},
        output      = #task_output{data = Data, next_activities = NextActivities},
        failed      = no} = Task,
  
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "task." ++ Name ++ ".status.current",         "done",
                       "task." ++ Name ++ ".status.done.time",       eh_datetime:now(),
                       "task." ++ Name ++ ".output.data",            Data,
                       "task." ++ Name ++ ".output.next_activities", NextActivities]),

  % now it is no longer running
  1 = redis(["del", redis_running_tag_for(UUIDTag, Name)]),

  % it is done
  ok = redis(["set", redis_done_tag_for(UUIDTag, Name), "yes"]),

  {noreply, State};

% save an error message happened in a job's task
handle_cast({save_error_on_task_execution, #task{} = Task}, State) ->
  #task{context_job = Job,
        activity    = #activity_definition{name = Name},
        output      = #task_output{data = Data, next_activities = _NextActivities},
        failed      = yes} = Task,

  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "task." ++ Name ++ ".status.current",    "error",
                       "task." ++ Name ++ ".status.error.time", eh_datetime:now(),
                       "task." ++ Name ++ ".output.data",       Data]),

  % now it is no longer running
  1 = redis(["del", redis_running_tag_for(UUIDTag, Name)]),

  % it failed
  ok = redis(["set", redis_error_tag_for(UUIDTag, Name), Data]),

  {noreply, State};

% mark a job as done
handle_cast({mark_job_as_done, #job{} = Job}, State) ->
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "job.status.current",   "done",
                       "job.status.done.time", eh_datetime:now()]),
                       
  % now it is no longer running
  1 = redis(["del", redis_running_tag_for(UUIDTag)]),

  % it is done
  ok = redis(["set", redis_done_tag_for(UUIDTag), UUIDTag]),

  ?DEBUG("----- JOB WAS MARKED AS DONE (~s) -----", [Job#job.uuid]),

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
  ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

% --- redis ---------------------------------------------------------------------------------------

redis(Command) ->
  Output = redo:cmd(cameron_redo, Command),
  eh_maybe:maybe_ok(eh_maybe:maybe_string(Output)).

redis_process_tag_for(ProcessName) ->
  % cameron:process:{name}:
  re:replace("cameron:process:{name}:", "{name}", eh_maybe:maybe_string(ProcessName), [{return, list}]).

redis_job_tag_for(ProcessName, Key, UUID) ->
  % cameron:process:{name}:key:{key}:job:{uuid}
  redis_process_tag_for(ProcessName) ++ "key:" ++ Key ++ ":job:" ++ UUID.
  
redis_job_tag_for(#job{} = Job) ->
  #job{uuid    = UUID,
       process = #process_definition{name = ProcessName},
       input   = #job_input{key = Key}} = Job,
    
  redis_job_tag_for(ProcessName, Key, UUID).

redis_pending_tag_for(AnyTag) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:pending
  AnyTag ++ ":pending".

redis_running_tag_for(AnyTag) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:running
  AnyTag ++ ":running".

redis_done_tag_for(AnyTag) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:done
  AnyTag ++ ":done".

redis_running_tag_for(UUIDTag, ActivityName) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:{activity}:running
  UUIDTag ++ ":" ++ ActivityName ++ ":running".

redis_done_tag_for(UUIDTag, ActivityName) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:{activity}:done
  UUIDTag ++ ":" ++ ActivityName ++ ":done".

redis_error_tag_for(UUIDTag, ActivityName) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:{activity}:error
  UUIDTag ++ ":" ++ ActivityName ++ ":error".

% --- job -----------------------------------------------------------------------------------------

new_uuid() ->
  eh_uuid:new().

extract_job_data(Job, Key, UUID) ->
  UUIDTag = redis_job_tag_for(Job, Key, UUID),
  RawData = redis(["hgetall", UUIDTag]),
  eh_list:to_properties(RawData).
  