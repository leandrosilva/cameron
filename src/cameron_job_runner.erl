%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The gen_server responsable to execute a process.

-module(cameron_job_runner).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/2, stop/1]).
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

% when a individual task is being handled
handle_cast({event, task_is_being_handled, #task{} = Task}, State) ->
  print_event("(event, task_is_being_handled)", Task, State),
  _NewState = update_state(task_is_being_handled, State);

% when a individual task has been done with no error
handle_cast({event, task_has_been_done, #task{} = Task}, State) ->
  print_event("(event, task_has_been_done)", Task, State),
  ok = cameron_job_data:save_task_output(Task),
  _NewState = update_state(task_has_been_done, State);

% when a individual task has been done with no error and has next activities/tasks (sub ones)
handle_cast({event, {task_has_been_done, has_next}, #task{} = Task}, State) ->
  print_event("(event, {task_has_been_done, has_next})", Task, State),
  ok = cameron_job_data:save_task_output(Task),
  _NewState = update_state({task_has_been_done, has_next}, State);

% when a individual task has been done with error
handle_cast({event, {task_has_been_done, with_error}, #task{} = Task}, State) ->
  print_event("(event, {task_has_been_done, with_error})", Task, State),
  ok = cameron_job_data:save_error_on_task_execution(Task),
  _NewState = update_state({task_has_been_done, with_error}, State);

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
  io:format("[cameron_job_runner] info :: JobUUID: ~s // EXIT: ~w ~w (N: ~w)~n", [JobUUID, Pid, Reason, N]),
  {noreply, State};
  
% down
handle_info({'DOWN',  Ref, Type, Pid, Info}, State) ->
  #job{uuid = JobUUID} = State#state.running_job,
  io:format("[cameron_job_runner] info :: JobUUID: ~s // DOWN: ~w ~w ~w ~w~n", [JobUUID, Ref, Type, Pid, Info]),
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
  io:format("[cameron_job_runner] terminating :: JobUUID: ~s // normal ~w (N: ~w)~n", [JobUUID, self(), N]),
  terminated;

% handle_info generic fallback (ignore) // any reason, i.e: cameron_process_sup:stop_child
terminate(Reason, State) ->
  #job{uuid = JobUUID} = State#state.running_job,
  io:format("[cameron_job_runner] terminating :: JobUUID: ~s // ~w~n", [JobUUID, Reason]),
  terminate.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

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
  spawn_link(?MODULE, handle_task, [Task]).

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
      
      case build_next_tasks(DoneTask#task.context_job, ResponseData, ResponseName, ResponseNextActivities) of
        undefined ->
          notify_event(task_has_been_done, DoneTask);
        Tasks when is_list(Tasks) ->
          run_parallel_tasks(Tasks),
          notify_event({task_has_been_done, has_next}, DoneTask)
      end;
    {ok, {{"HTTP/1.1", _, _}, _, ResponsePayload}} ->
      FailedTask = Task#task{output = #task_output{data = ResponsePayload}, failed = yes},
      notify_event({task_has_been_done, with_error}, FailedTask);
    {error, {connect_failed, emfile}} ->
      FailedTask = Task#task{output = #task_output{data = "{connect_failed, emfile}"}, failed = yes},
      notify_event({task_has_been_done, with_error}, FailedTask);
    {error, econnrefused} ->
      FailedTask = Task#task{output = #task_output{data = ["{econnrefused, ", URL, "}"]}, failed = yes},
      notify_event({task_has_been_done, with_error}, FailedTask);
    {error, Reason} ->
      io:format("[cameron_job_runner] handle_task :: http response // ERROR: Reason = ~w~n", [Reason]),
      FailedTask = Task#task{output = #task_output{data = "unknown_error"}, failed = yes},
      notify_event({task_has_been_done, with_error}, FailedTask)
  end,
  
  ok.

notify_event(Event, #task{} = Task) ->
  Job = Task#task.context_job,

  Pname = ?pname(Job#job.uuid),
  ok = gen_server:cast(Pname, {event, Event, Task}).
  
update_state(task_is_being_handled, State) ->
  N = State#state.how_many_running_tasks,
  NewState = State#state{how_many_running_tasks = N + 1},
  {noreply, NewState};
  
update_state(task_has_been_done, State) ->
  update_state(State);

update_state({task_has_been_done, has_next}, State) ->
  N = State#state.how_many_running_tasks,
  NewState = State#state{how_many_running_tasks = N - 1},
  {noreply, NewState};

update_state({task_has_been_done, with_error}, State) ->
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
  
print_event(Event, Task, State) ->
  #task{activity = #activity_definition{name = Name}} = Task,
  N = State#state.how_many_running_tasks,
  io:format("[cameron_job_runner] ~s :: task: ~s (~w // N: ~w)~n", [Event, Name, self(), N]).
  