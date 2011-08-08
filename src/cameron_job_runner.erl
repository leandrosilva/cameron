%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The gen_server responsable to execute a process instance, which we call job.

-module(cameron_job_runner).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/2, dump/1, stop/1]).
% public api
-export([run_job/1, handle_task/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("cameron.hrl").

-record(state, {running_job, how_many_running_tasks}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(Pname, Job) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start a cameron_job_runner generic server. Pname is the server "process name", or in other
%%      words, the name by which it is going to be registered.
start_link(Pname, Job) ->
  gen_server:start_link({local, Pname}, ?MODULE, [Job], []).

%% @spec dump(Pname) -> {ok, ServerDump} | {error, Error}
%% @doc Dumps generic server state. Pname is the server "process name", or in other words, the name
%%      by which it was registered.
dump(Pname) ->
  gen_server:cast(Pname, dump).

%% @spec stop(Pname) -> ok
%% @doc Manually stops the server. Pname is the server "process name", or in other words, the name
%%      by which it was registered.
stop(Pname) ->
  gen_server:cast(Pname, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec run_job(Job) -> ok
%% @doc Create a new process instance (a.k.a. job), child of cameron_process_sup, and then run it
%%      in parallel.
run_job(#job{} = Job) ->
  case cameron_process_sup:start_child(Job) of
    {ok, _Pid} ->
      ok = dispatch_action(run_job, Job);
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

% to run a job
handle_cast({action, run_job}, State) ->
  Job = State#state.running_job,
  ok = cameron_job_data:mark_job_as_running(Job),

  StartTask = build_start_task(Job),
  dispatch_action(spawn_task, StartTask),
  
  {noreply, State};

% to spawn a individual task handler
handle_cast({action, spawn_task, #task{} = Task}, State) ->
  Job = State#state.running_job,
  #job{uuid = UUID} = Job,
  log_action(UUID, {spawn_task, #task{} = Task}, State),
  spawn_link(?MODULE, handle_task, [Job, Task]),
  _NewState = update_state(task_has_been_spawned, State);

% when a individual task is being handled
handle_cast({event, task_is_being_handled, #task{} = Task}, State) ->
  #job{uuid = UUID} = State#state.running_job,
  log_event(UUID, {task_is_being_handled, #task{} = Task}, State),
  ok = cameron_job_data:mark_task_as_running(Task),
  _NewState = update_state(task_is_being_handled, State);

% when a individual task has been done with "no error"
handle_cast({event, task_has_been_done, #task{} = Task}, State) ->
  #job{uuid = UUID} = State#state.running_job,
  log_event(UUID, {task_has_been_done, #task{} = Task}, State),
  ok = cameron_job_data:save_task_output(Task),
  _NewState = update_state(task_has_been_done, State);

% when a individual task has been done with error
handle_cast({event, task_has_been_done_with_error, #task{} = Task}, State) ->
  #job{uuid = UUID} = State#state.running_job,
  log_event(UUID, {task_has_been_done_with_error, #task{} = Task}, State),
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

% exit // by any reason
handle_info({'EXIT', Pid, Reason}, State) ->
  % i could do 'how_many_running_tasks' and mark_job_as_done here, couldn't i?
  #job{uuid = UUID} = State#state.running_job,
  N = State#state.how_many_running_tasks,
  log_info(UUID, {Pid, Reason, N}),
  {noreply, State};
  
% down
handle_info({'DOWN',  Ref, Type, Pid, Info}, State) ->
  #job{uuid = UUID} = State#state.running_job,
  N = State#state.how_many_running_tasks,
  log_info(UUID, {Pid, N, Ref, Type, Info}),
  {noreply, State};
  
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.

% no problem, that's ok
terminate(normal, State) ->
  #job{uuid = UUID} = State#state.running_job,
  N = State#state.how_many_running_tasks,
  log_termination(UUID, {self(), N}),
  ok;

% handle_info generic fallback (ignore) // any reason, i.e: cameron_process_sup:stop_child
terminate(Reason, State) ->
  #job{uuid = UUID} = State#state.running_job,
  N = State#state.how_many_running_tasks,
  log_termination(UUID, {self(), N, Reason}),
  ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

% --- gen_server message dispatching --------------------------------------------------------------

dispatch_action(_, undefined) ->
  undefined;

dispatch_action(run_job, Job) ->
  dispatch_message(Job, {action, run_job});
  
dispatch_action(spawn_tasks, Tasks) when is_list(Tasks) ->
  SpawnTask = fun (Task) ->
                dispatch_action(spawn_task, Task)
              end,
  lists:map(SpawnTask, Tasks);
  
dispatch_action(Action, #task{} = Task) ->
  dispatch_message({action, Action, Task}).
  
dispatch_event(Event, #task{} = Task) ->
  dispatch_message({event, Event, Task}).
  
dispatch_message({Type, What, #task{} = Task}) ->
  Job = Task#task.context_job,
  ok = dispatch_message(Job, {Type, What, Task}).
  
dispatch_message(Job, Message) ->
  Pname = ?pname(Job#job.uuid),
  ok = gen_server:cast(Pname, Message).
  
% --- gen_server state management -----------------------------------------------------------------
  
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

% --- task building -------------------------------------------------------------------------------

build_task(Job, {Data, Requestor}, ActivityDefinition) ->
  #job{input = #job_input{key = Key}} = Job,
  
  TaskInput = #task_input{key       = Key,
                          data      = Data,
                          requestor = Requestor},

  #task{context_job = Job,
        activity    = ActivityDefinition,
        input       = TaskInput}.

build_start_task(Job) ->
  #job{process = #process_definition{start_activity = StartActivityDefinition},
       input   = JobInput} = Job,

  #job_input{data      = Data,
             requestor = Requestor} = JobInput,
  
  build_task(Job, {Data, Requestor}, StartActivityDefinition).

build_next_task(Job, Data, Requestor, ActivityDefinition) ->
  build_task(Job, {Data, Requestor}, ActivityDefinition).

build_next_tasks(_Job, _Data, _Requestor, undefined) ->
  undefined;
  
build_next_tasks(Job, Data, Requestor, NextActivitiesJson) ->
  NextActivitiesStruct = struct:from_json(NextActivitiesJson),
  ActivitiesStruct = struct:get_value(<<"definitions">>, NextActivitiesStruct),

  BuildNextTask = fun (ActivityStruct) ->
                    Name = struct:get_value(<<"name">>, ActivityStruct, {format, list}),
                    URL = struct:get_value(<<"url">>, ActivityStruct, {format, list}),

                    build_next_task(Job,
                                    Data,
                                    Requestor,
                                    #activity_definition{name = Name,
                                                         url  = URL})
                  end,

  lists:map(BuildNextTask, ActivitiesStruct).

build_failed_task(Task, {Key, Value}) when is_atom(Key) and is_atom(Value) ->
  build_failed_task(Task, [atom_to_list(Key), " - ", atom_to_list(Value)]);
  
build_failed_task(Task, Reason) when is_atom(Reason) ->
  build_failed_task(Task, atom_to_list(Reason));
  
build_failed_task(Task, Reason) ->
  #task{context_job = #job{uuid = UUID},
        activity = #activity_definition{url = URL}} = Task,
        
  Error = ["{\"error\":\"", Reason, "\",\"url\":\"", URL, "\"}"],
  log_failed_task(UUID, {Task, Error}),
  Task#task{output = #task_output{data = Error}, failed = yes}.

% --- task handling -------------------------------------------------------------------------------

handle_task(Job, #task{} = Task) ->
  dispatch_event(task_is_being_handled, Task),
  
  #task{activity = #activity_definition{url = URL},
        input    = #task_input{data = Data, requestor = Requestor}} = Task,

  RequestPayload = build_request_payload(Job, {Data, Requestor}),
  
  case execute_task(Task, {http_request, URL, RequestPayload}) of
    {task_has_been_done, DoneTask, NextTasks} ->
      dispatch_action(spawn_tasks, NextTasks),
      dispatch_event(task_has_been_done, DoneTask);
    {task_has_been_done_with_error, FailedTask} ->
      dispatch_event(task_has_been_done_with_error, FailedTask)
  end,
  
  ok.

execute_task(Task, {http_request, URL, RequestPayload}) ->
  HttpResponse = eh_http:http_post(URL, RequestPayload),
  inspect_task_result(Task, HttpResponse).
  
inspect_task_result(Task, {ok, {{"HTTP/1.1", 200, _}, _, ResponsePayload}}) ->
  {ResponseName, ResponseData, ResponseNextActivities} = parse_response_payload(ResponsePayload),
  DoneTask = Task#task{output = #task_output{data = ResponseData, next_activities = ResponseNextActivities}},
  NextTasks = build_next_tasks(DoneTask#task.context_job, ResponseData, ResponseName, ResponseNextActivities),
  {task_has_been_done, DoneTask, NextTasks};

inspect_task_result(Task, {ok, {{"HTTP/1.1", _, _}, _, ResponsePayload}}) ->
  FailedTask = Task#task{output = #task_output{data = ResponsePayload}, failed = yes},
  {task_has_been_done_with_error, FailedTask};
  
inspect_task_result(Task, {error, Reason}) ->
  ?DEBUG("cameron_job_runner >> func: handle_task, http_response: (ERROR) ~w~n", [Reason]),
  FailedTask = build_failed_task(Task, Reason),
  {task_has_been_done_with_error, FailedTask}.
  
% --- how to build task payload (from and to json) ------------------------------------------------

build_request_payload(Job, {Data, Requestor}) ->
  #job{uuid  = UUID,
       input = #job_input{key = Key}} = Job,

  RequestPayload = struct:to_json({struct, [{<<"job">>, list_to_binary(UUID)},
                                            {<<"key">>, list_to_binary(Key)},
                                            {<<"data">>, list_to_binary(Data)},
                                            {<<"requestor">>, list_to_binary(Requestor)}]}),
                                        
  unicode:characters_to_list(RequestPayload).

parse_response_payload(ResponsePayload) ->
  Struct = struct:from_json(ResponsePayload),

  Name = struct:get_value(<<"name">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, json}),
  NextActivities = struct:get_value(<<"next_activities">>, Struct, {format, json}),
  
  {Name, Data, NextActivities}.
  
% --- log -----------------------------------------------------------------------------------------

log_action(UUID, {Action, Task}, State) ->
  #task{activity = #activity_definition{name = Name}} = Task,
  N = State#state.how_many_running_tasks,
  ?DEBUG("cameron_job_runner >> (~w, N: ~w) action: ~w, UUID: ~s, task: ~s", [self(), N, Action, UUID, Name]).

log_event(UUID, {Event, Task}, State) ->
  #task{activity = #activity_definition{name = Name}} = Task,
  N = State#state.how_many_running_tasks,
  ?DEBUG("cameron_job_runner >> (~w, N: ~w) event: ~w, UUID: ~s, task: ~s", [self(), N, Event, UUID, Name]).

log_info(UUID, {Pid, Reason, N}) ->
  ?DEBUG("cameron_job_runner >> (~w, N: ~w) info: exit, UUID: ~s, reason: ~w", [Pid, N, UUID, Reason]);
  
log_info(UUID, {Pid, N, Ref, Type, Info}) ->
  ?DEBUG("cameron_job_runner >> (~w, N: ~w) info: down, UUID: ~s, ref: ~w, type: ~w, info: ~w", [Pid, N, UUID, Ref, Type, Info]).
  
log_termination(UUID, {Pid, N}) ->
  ?DEBUG("cameron_job_runner >> (~w, N: ~w) termination: normal, UUID: ~s", [Pid, N, UUID]);
  
log_termination(UUID, {Pid, N, Reason}) ->
  ?DEBUG("cameron_job_runner >> (~w, N: ~w) termination: ~w, UUID: ~s", [Pid, N, Reason, UUID]).
  
log_failed_task(UUID, {Task, Error}) ->
  #task{activity = #activity_definition{name = Name}} = Task,
  ?DEBUG("cameron_job_runner >> (~w) failing: on_task, UUID: ~s, task: ~s, error: ~w", [self(), UUID, Name, Error]).
  