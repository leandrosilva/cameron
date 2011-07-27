%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The gen_server responsable to execute a process.

-module(cameron_job_runner).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/2, dump/1, stop/1]).
% public api
-export([run_job/1, handle_task/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {running_job, how_many_running_tasks}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(Pname, Job) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start a cameron_process server.
start_link(Pname, Job) ->
  gen_server:start_link({local, Pname}, ?MODULE, [Job], []).

%% @spec dump(Pname) -> {ok, ServerDump} | {error, Error}
%% @doc Dumps server state.
dump(Pname) ->
  gen_server:cast(Pname, dump).

%% @spec stop(Pname) -> ok
%% @doc Manually stops the server.
stop(Pname) ->
  gen_server:cast(Pname, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec mark_job_as_running(Job) -> ok
%% @doc Create a new process, child of cameron_process_sup, and then run the process (in
%%      parallel, of course) to the job given.
run_job(#job{uuid = JobUUID} = Job) ->
  case cameron_process_sup:start_child(Job) of
    {ok, _Pid} ->
      ok = gen_server:cast(?pname(JobUUID), run_job);
    {error, {already_started, _Pid}} ->
      ok
  end.

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init([Job]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init([Job]) ->
  process_flag(trap_exit, true),
  
  {ok, #state{running_job = Job, how_many_running_tasks = 0}}.

%% @spec handle_call(Job, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% handle_call generic fallback
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

%% @spec handle_cast(Msg, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling cast messages.

% wake up to run a process
handle_cast(run_job, State) ->
  Job = State#state.running_job,
  ok = cameron_job_data:mark_job_as_running(Job),

  StartTask = build_start_task(Job),
  run_parallel_task(StartTask),
  
  {noreply, State};

% when a individual task is being spawned
handle_cast({action, run_parallel_task, #task{} = Task}, State) ->
  log_action({action, run_parallel_task, #task{} = Task}, State),
  spawn_link(?MODULE, handle_task, [Task]),
  _NewState = update_state(task_has_been_spawned, State);

% when a individual task is being handled
handle_cast({event, task_is_being_handled, #task{} = Task}, State) ->
  log_event({event, task_is_being_handled, #task{} = Task}, State),
  _NewState = update_state(task_is_being_handled, State);

% when a individual task has been done with no error
handle_cast({event, task_has_been_done, #task{} = Task}, State) ->
  log_event({event, task_has_been_done, #task{} = Task}, State),
  ok = cameron_job_data:save_task_output(Task),
  _NewState = update_state(task_has_been_done, State);

% when a individual task has been done with error
handle_cast({event, task_has_been_done_with_error, #task{} = Task}, State) ->
  log_event({event, task_has_been_done_with_error, #task{} = Task}, State),
  ok = cameron_job_data:save_error_on_task_execution(Task),
  _NewState = update_state(task_has_been_done_with_error, State);

% dumps server state
handle_cast(dump, State) ->
  {state, {job, UUID,                                                                                    
                {process_definition, ProcessName,                                                        
                                     {activity_definition, StartActivity, URL}},                         
                {job_input, Key, Data, Requestor}},                                                      
          HowManyTasksRunning} = State,
                   
  ?NOTICE("cameron_job_runner >> current state:~n
    {state, {job, {uuid, ~s},
                  {process_definition, {name, ~s},
                                       {activity_definition, {name, ~s},
                                                             {url, ~s}}},
                  {job_input, {key, ~s},
                              {data, ~s},
                              {requestor, ~s}}},
            {how_many_running_tasks, ~w}}", [UUID, ProcessName, StartActivity, URL,
                                             Key, Data, Requestor, HowManyTasksRunning]),
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

% exit // any reason
handle_info({'EXIT', Pid, Reason}, State) ->
  % i could do 'how_many_running_tasks' and mark_job_as_done here, couldn't i?
  #job{uuid = JobUUID} = State#state.running_job,
  N = State#state.how_many_running_tasks,
  ?DEBUG("cameron_job_runner >> handling: info, JobUUID: ~s // EXIT: ~w ~w (N: ~w)", [JobUUID, Pid, Reason, N]),
  {noreply, State};
  
% down
handle_info({'DOWN',  Ref, Type, Pid, Info}, State) ->
  #job{uuid = JobUUID} = State#state.running_job,
  ?DEBUG("cameron_job_runner >> handling: info, JobUUID: ~s // DOWN: ~w ~w ~w ~w", [JobUUID, Ref, Type, Pid, Info]),
  {noreply, State};
  
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.

% no problem, that's ok
terminate(normal, State) ->
  #job{uuid = JobUUID} = State#state.running_job,
  N = State#state.how_many_running_tasks,
  ?DEBUG("cameron_job_runner >> handling: terminate, JobUUID: ~s // normal ~w (N: ~w)", [JobUUID, self(), N]),
  terminated;

% handle_info generic fallback (ignore) // any reason, i.e: cameron_process_sup:stop_child
terminate(Reason, State) ->
  #job{uuid = JobUUID} = State#state.running_job,
  ?DEBUG("cameron_job_runner >> handling: terminate, JobUUID: ~s // ~w", [JobUUID, Reason]),
  terminate.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

% task build and handling

build_task(ContextJob, {Key, Data, Requestor}, ActivityDefinition) ->
  TaskInput = #task_input{key       = Key,
                          data      = Data,
                          requestor = Requestor},
  
  #task{context_job = ContextJob,
        activity    = ActivityDefinition,
        input       = TaskInput}.

build_start_task(ContextJob) ->
  #job{process = #process_definition{start_activity = StartActivityDefinition},
       input   = JobInput} = ContextJob,

  #job_input{key       = Key,
             data      = Data,
             requestor = Requestor} = JobInput,
  
  build_task(ContextJob, {Key, Data, Requestor}, StartActivityDefinition).

build_next_task(ContextJob, Data, Requestor, ActivityDefinition) ->
  #job{input = #job_input{key = Key}} = ContextJob,
  build_task(ContextJob, {Key, Data, Requestor}, ActivityDefinition).

build_next_tasks(_ContextJob, _Data, _Requestor, undefined) ->
  undefined;
  
build_next_tasks(ContextJob, Data, Requestor, NextActivitiesJson) ->
  NextActivitiesStruct = struct:from_json(NextActivitiesJson),
  Parallelizable = struct:get_value(<<"parallelizable">>, NextActivitiesStruct, {format, atom}),
  ActivitiesStruct = struct:get_value(<<"definitions">>, NextActivitiesStruct),

  BuildNextTask = fun (ActivityStruct) ->
                    Name = struct:get_value(<<"name">>, ActivityStruct, {format, list}),
                    URL = struct:get_value(<<"url">>, ActivityStruct, {format, list}),

                    build_next_task(ContextJob,
                                    Data,
                                    Requestor,
                                    #activity_definition{name = Name, url = URL})
                  end,

  lists:map(BuildNextTask, ActivitiesStruct).

run_parallel_task(Task) ->
  ask_action(run_parallel_task, Task).

run_parallel_tasks(undefined) ->
  undefined;

run_parallel_tasks(Tasks) ->
  RunParallelTask = fun (Task) ->
                      run_parallel_task(Task)
                    end,
                    
  lists:map(RunParallelTask, Tasks).

handle_task(#task{} = Task) ->
  notify_event(task_is_being_handled, Task),
  
  #task{activity = #activity_definition{url = URL},
        input    = #task_input{key = Key, data = Data, requestor = Requestor}} = Task,

  RequestPayload = build_request_payload(Key, Data, Requestor),

  case http_helper:http_post(URL, RequestPayload) of
    {ok, {{"HTTP/1.1", 200, _}, _, ResponsePayload}} ->
      {ResponseName, ResponseData, ResponseNextActivities} = parse_response_payload(ResponsePayload),
      DoneTask = Task#task{output = #task_output{data = ResponseData, next_activities = ResponseNextActivities}},

      NextTasks = build_next_tasks(DoneTask#task.context_job, ResponseData, ResponseName, ResponseNextActivities),
      run_parallel_tasks(NextTasks),

      notify_event(task_has_been_done, DoneTask);
    {ok, {{"HTTP/1.1", _, _}, _, ResponsePayload}} ->
      FailedTask = Task#task{output = #task_output{data = ResponsePayload}, failed = yes},
      notify_event(task_has_been_done_with_error, FailedTask);
    {error, {connect_failed, emfile}} ->
      FailedTask = Task#task{output = #task_output{data = "{connect_failed, emfile}"}, failed = yes},
      notify_event(task_has_been_done_with_error, FailedTask);
    {error, econnrefused} ->
      FailedTask = Task#task{output = #task_output{data = ["{econnrefused, ", URL, "}"]}, failed = yes},
      notify_event(task_has_been_done_with_error, FailedTask);
    {error, Reason} ->
      ?DEBUG("cameron_job_runner >> func: handle_task, http_response: (ERROR) ~w~n", [Reason]),
      FailedTask = Task#task{output = #task_output{data = "unknown_error"}, failed = yes},
      notify_event(task_has_been_done_with_error, FailedTask)
  end,
  
  ok.

% gen_server message dispatch

ask_action(Action, #task{} = Task) ->
  dispatch_message({action, Action, Task}).

notify_event(Event, #task{} = Task) ->
  dispatch_message({event, Event, Task}).
  
dispatch_message({Type, What, #task{} = Task}) ->
  Job = Task#task.context_job,
  Pname = ?pname(Job#job.uuid),
  ok = gen_server:cast(Pname, {Type, What, Task}).
  
% gen_server state
  
update_state(task_has_been_spawned, State) ->
  N = State#state.how_many_running_tasks,
  NewState = State#state{how_many_running_tasks = N + 1},
  {noreply, NewState};

update_state(task_is_being_handled, State) ->
  {noreply, State};
  
update_state(task_has_been_done, State) ->
  update_state(State);

update_state(task_has_been_done_with_error, State) ->
  update_state(State).

update_state(State) ->
  case State#state.how_many_running_tasks of
    1 ->
      ok = cameron_job_data:mark_job_as_done(State#state.running_job),
      NewState = State#state{how_many_running_tasks = 0},
      {stop, normal, NewState};
    N ->
      NewState = State#state{how_many_running_tasks = N - 1},
      {noreply, NewState}
  end.

% how to build task payload (from and to json)

build_request_payload(Key, Data, Requestor) ->
  RequestPayload = struct:to_json({struct, [{<<"key">>, list_to_binary(Key)},
                                            {<<"data">>, list_to_binary(Data)},
                                            {<<"requestor">>, list_to_binary(Requestor)}]}),
                                        
  unicode:characters_to_list(RequestPayload).

parse_response_payload(ResponsePayload) ->
  Struct = struct:from_json(ResponsePayload),

  Name = struct:get_value(<<"name">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, json}),
  NextActivities = struct:get_value(<<"next_activities">>, Struct, {format, json}),
  
  {Name, Data, NextActivities}.
  
% log

log_action({action, Action, Task}, State) ->
  #task{activity = #activity_definition{name = Name}} = Task,
  N = State#state.how_many_running_tasks,
  ?DEBUG("cameron_job_runner >> action: ~w, task: ~s (~w // N: ~w)", [Action, Name, self(), N]).

log_event({event, Event, Task}, State) ->
  #task{activity = #activity_definition{name = Name}} = Task,
  N = State#state.how_many_running_tasks,
  ?DEBUG("cameron_job_runner >> event: ~w, task: ~s (~w // N: ~w)", [Event, Name, self(), N]).
  