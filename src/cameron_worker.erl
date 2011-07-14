%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The worker gen_server, the responsable to make diagnostic.

-module(cameron_worker).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/1]).
% public api
-export([pay_it/1, get_name/1, work/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {promise_uuid, countdown}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(Promise) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start a cameron_worker server.
start_link(Promise) ->
  WorkerName = cameron_worker_sup:which_child(Promise),
  gen_server:start_link({local, WorkerName}, ?MODULE, [Promise], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop(Promise) ->
  WorkerName = get_name(Promise),
  gen_server:cast(WorkerName, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec pay_it(Promise) -> ok
%% @doc Create a new process, child of cameron_worker_sup, and then make a complete diagnostic (in
%%      parallel, of course) to the promise given.
pay_it(#promise{uuid = PromiseUUID} = Promise) ->
  case cameron_worker_sup:start_child(PromiseUUID) of
    {ok, _Pid} ->
      WorkerName = get_name(PromiseUUID),
      ok = gen_server:cast(WorkerName, {pay_it, Promise});
    {error, {already_started, _Pid}} ->
      ok
  end,
  ok.

%% @spec get_name(PromiseUUID) -> WorkerName
%% @doc Which worker is handling a promise given.
get_name(PromiseUUID) ->
  cameron_worker_sup:which_child(PromiseUUID).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(PromiseUUID) ->
  {ok, #state{promise_uuid = PromiseUUID}}.

%% @spec handle_call(Promise, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% handle_call generic fallback
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

%% @spec handle_cast(Msg, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling cast messages.

% wake up to run a workflow
handle_cast({pay_it, #promise{uuid = PromiseUUID} = Promise}, State) ->
  io:format("~n--- [cameron_worker] paying // Promise: ~w~n", [Promise]),
  
  CloudInput = #step_input{promise     = Promise,
                           name        = "cloud_zabbix",
                           url         = "http://localhost:9292/workflow/v0.0.1/cloud/zabbix",
                           payload     = "xxx",
                           worker_name = get_name(PromiseUUID)},
  
  spawn(?MODULE, work, [1, CloudInput]),
  
  HostInput = #step_input{promise     = Promise,
                          name        = "hosting_zabbix",
                          url         = "http://localhost:9292/workflow/v0.0.1/hosting/zabbix",
                          payload     = "yyy",
                          worker_name = get_name(PromiseUUID)},
  
  spawn(?MODULE, work, [2, HostInput]),
  
  SqlServerInput = #step_input{promise     = Promise,
                               name        = "sqlserver_zabbix",
                               url         = "http://localhost:9292/workflow/v0.0.1/sqlserver/zabbix",
                               payload     = "zzz",
                               worker_name = get_name(PromiseUUID)},
  
  spawn(?MODULE, work, [3, SqlServerInput]),
  
  {noreply, State#state{countdown = 3}};

% notify when a individual diagnostic is done
handle_cast({notify_done, Index, #step_output{step_input = StepInput} = StepOutput}, State) ->
  io:format("--- [~s] Index: ~w, WorkerName: ~s // Notified its work is done~n", [StepInput#step_input.name,
                                                                                  Index,
                                                                                  StepInput#step_input.worker_name]),
  
  {ok, Promise} = cameron_workflow:save_progress(StepOutput),

  case State#state.countdown of
    1 ->
      {ok, Promise} = cameron_workflow:mark_as_paid(Promise),
      NewState = State#state{countdown = 0},
      {stop, normal, NewState};
    N ->
      NewState = State#state{countdown = N - 1},
      {noreply, NewState}
  end;

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
terminate(Reason, State) ->
  io:format("~n--- [cameron_worker] terminating // Promise: ~s, Reason: ~w~n", [State#state.promise_uuid, Reason]),
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

work(Index, #step_input{promise     = Promise,
                        name        = Name,
                        url         = URL,
                        payload     = _Payload,
                        worker_name = WorkerName} = StepInput) ->
  io:format("--- [(~w) cameron_worker_~s] Index: ~w, WorkerName: ~s, Name: ~s~n", [self(),
                                                                                   Promise#promise.uuid,
                                                                                   Index, 
                                                                                   WorkerName, 
                                                                                   Name]),

  % {ok, {{"HTTP/1.1", 200, "OK"},
  %       [_, _, _, _, _, _, _, _, _, _, _],
  %       Result}} = http_helper:http_get(URL),

  {ok, {{"HTTP/1.1", 200, _}, _, Output}} = http_helper:http_get(URL),
         
  StepOutput = #step_output{step_input = StepInput, output = Output},

  io:format("--- [(~w) cameron_worker_~s] Index: ~w, WorkerName: ~s, Name: ~s // Paid~n", [self(),
                                                                                           Promise#promise.uuid,
                                                                                           Index, 
                                                                                           WorkerName, 
                                                                                           Name]),
  
  notify_done(Index, StepOutput),
  ok.
  
notify_done(Index, #step_output{step_input = StepInput} = StepOutput) ->
  Promise = StepInput#step_input.promise,
  
  WorkerName = get_name(Promise#promise.uuid),
  ok = gen_server:cast(WorkerName, {notify_done, Index, StepOutput}).
  