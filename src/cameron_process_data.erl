%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for requests. It's used to keep track in a database every state (activities?) of a
%%      request that refers to a request to run a process. And in this case, it stores those activities
%%      in a Redis server.

-module(cameron_process_data).
-author('Leandro Silva <leandrodoze@gmail.com>').

% admin api
-export([start_link/0, stop/0]).
% public api
-export([create_new_job/2, mark_job_as_running/1]).
-export([save_activity_output/1, save_error_on_activity_execution/1, mark_job_as_done/1]).
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

%% @spec create_new_job(Process, {Key, Input, Requestor}) -> {ok, Job} | {error, Reason}
%% @doc The first activity of the whole process is accept a request and create a job which should run
%%      and keep track of process execution state and get any related data at any time in the
%%      future.
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
%% @doc Status: running, which means it's work in progress.
mark_job_as_running(#job{} = Job) ->
  ok = gen_server:cast(?MODULE, {mark_job_as_running, Job}).

%% @spec save_activity_output(Activity) -> ok | {error, Reason}
%% @doc Save to Redis a process execution output.
%%      Status: done, for this particular activity.
save_activity_output(#activity{} = Activity) ->
  ok = gen_server:cast(?MODULE, {save_activity_output, Activity}).

%% @spec save_error_on_activity_execution(Activity) -> ok | {error, Reason}
%% @doc Status: error, but the job could be done.
save_error_on_activity_execution(#activity{} = Activity) ->
  ok = gen_server:cast(?MODULE, {save_activity_output, Activity}).

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
handle_cast({create_new_job, #job{} = NewJob}, State) ->
  #job_input{key       = Key,
             data      = Data,
             requestor = Requestor} = NewJob#job.input,
    
  UUIDTag = redis_job_tag_for(NewJob),

  ok = redis(["hmset", UUIDTag,
                       "job.key",          Key,
                       "job.data",         Data,
                       "job.requestor",    Requestor,
                       "status.current",   "scheduled",
                       "status.scheduled", datetime()]),
  
  ok = redis(["set", redis_pending_tag_for(UUIDTag), UUIDTag]),
  
  {noreply, State};

% mark a new job as running
handle_cast({mark_job_as_running, #job{} = Job}, State) ->
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "status.current",      "running",
                       "status.running.time", datetime()]),

  {noreply, State};

% save the result of a activity given
handle_cast({save_activity_output, #activity{} = Activity}, State) ->
  #activity{definition   = #activity_definition{name = Name},
            context_job  = Job,
            output       = #activity_output{data = Data, next_activities = _NextActivities},
            failed       = no} = Activity,
  
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "activity." ++ Name ++ ".status.current",   "done",
                       "activity." ++ Name ++ ".status.done.time", datetime(),
                       "activity." ++ Name ++ ".output.data",      Data]),

  {noreply, State};

% save an error message happened in a job payment process
handle_cast({save_error_on_activity_execution, #activity{} = Activity}, State) ->
  #activity{definition   = #activity_definition{name = Name},
            context_job  = Job,
            output       = #activity_output{data = Data, next_activities = _NextActivities},
            failed       = yes} = Activity,

  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "activity." ++ Name ++ ".status.current",    "error",
                       "activity." ++ Name ++ ".status.error.time", datetime(),
                       "activity." ++ Name ++ ".output.data",       Data]),

  ok = redis(["set", redis_error_tag_for(UUIDTag, Name), Data]),

  {noreply, State};

% mark a job as done
handle_cast({mark_job_as_done, #job{} = Job}, State) ->
  UUIDTag = redis_job_tag_for(Job),

  ok = redis(["hmset", UUIDTag,
                       "status.current",   "done",
                       "status.done.time", datetime()]),
                       
  io:format("~n~n----- NOW JOB IS MARKED AS DONE -----~n~n"),

  1 = redis(["del", redis_pending_tag_for(UUIDTag)]),
  ok = redis(["set", redis_done_tag_for(UUIDTag), UUIDTag]),

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

redis_process_tag_for(ProcessName) ->
  % cameron:process:{name}:
  re:replace("cameron:process:{name}:", "{name}", maybe_string(ProcessName), [{return, list}]).

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

redis_done_tag_for(AnyTag) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:done
  AnyTag ++ ":done".

redis_error_tag_for(UUIDTag, ActivityName) ->
  % cameron:process:{name}:key:{key}:job:{uuid}:error
  UUIDTag ++ ":" ++ ActivityName ++ ":error".

%% job

new_uuid() ->
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
  