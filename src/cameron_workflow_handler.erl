%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The gen_server responsable to execute a workflow.

-module(cameron_workflow_handler).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/2, stop/1]).
% public api
-export([handle_request/1, handle_promise/1, work/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {promise, countdown}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(Pname, Promise) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start a cameron_workflow server.
start_link(Pname, Promise) ->
  gen_server:start_link({local, Pname}, ?MODULE, [Promise], []).

%% @spec stop(Pname) -> ok
%% @doc Manually stops the server.
stop(Pname) ->
  gen_server:cast(Pname, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec handle_request(Request) -> {ok, Promise} | {error, Reason}
%% @doc It triggers an async dispatch of a resquest to run a workflow an pay a promise.
handle_request(#request{} = Request) ->
  {ok, _Promise} = cameron_workflow_persistence:save_new_request(Request).

%% @spec mark_promise_as_dispatched(Promise) -> ok
%% @doc Create a new process, child of cameron_workflow_sup, and then run the workflow (in
%%      parallel, of course) to the promise given.
handle_promise(#promise{uuid = PromiseUUID} = Promise) ->
  case cameron_workflow_sup:start_child(Promise) of
    {ok, _Pid} ->
      ok = gen_server:cast(?pname(PromiseUUID), {handle_promise, Promise});
    {error, {already_started, _Pid}} ->
      ok
  end.

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init([Promise]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init([Promise]) ->
  process_flag(trap_exit, true),
  {ok, #state{promise = Promise}}.

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
handle_cast({handle_promise, #promise{uuid = PromiseUUID} = Promise}, State) ->
  ok = cameron_workflow_persistence:mark_promise_as_dispatched(Promise),
  
  CloudInput = #step_input{promise = Promise,
                           name    = "cloud_zabbix",
                           url     = "http://eumoroemguarulhos.appspot.com",
                           payload = "xxx",
                           pname   = ?pname(PromiseUUID)},
  
  Pid1 = spawn_link(?MODULE, work, [1, CloudInput]),
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // Cloud Pid: ~w~n", [PromiseUUID, Pid1]),
  
  HostInput = #step_input{promise = Promise,
                          name    = "hosting_zabbix",
                          url     = "http://eumoroemguarulhos.appspot.com",
                          payload = "yyy",
                          pname   = ?pname(PromiseUUID)},
  
  Pid2 = spawn_link(?MODULE, work, [2, HostInput]),
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // Host Pid: ~w~n", [PromiseUUID, Pid2]),
  
  SqlServerInput = #step_input{promise = Promise,
                               name    = "sqlserver_zabbix",
                               url     = "http://eumoroemguarulhos.appspot.com",
                               payload = "zzz",
                               pname   = ?pname(PromiseUUID)},
  
  Pid3 = spawn_link(?MODULE, work, [3, SqlServerInput]),
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // SqlServer Pid: ~w~n", [PromiseUUID, Pid3]),
  
  {noreply, State#state{countdown = 3}};

% notify when a individual promise is done
handle_cast({notify_paid, _Index, #step_output{step_input = _StepInput} = StepOutput}, State) ->
  ok = cameron_workflow_persistence:save_promise_progress(StepOutput),

  case State#state.countdown of
    1 ->
      ok = cameron_workflow_persistence:mark_promise_as_paid(State#state.promise),
      NewState = State#state{countdown = 0},
      {stop, normal, NewState};
    N ->
      NewState = State#state{countdown = N - 1},
      {noreply, NewState}
  end;

% notify when a individual promise fail
handle_cast({notify_error, _Index, #step_output{step_input = _StepInput} = StepOutput}, State) ->
  ok = cameron_workflow_persistence:save_error_on_promise_progress(StepOutput),

  case State#state.countdown of
    1 ->
      ok = cameron_workflow_persistence:mark_promise_as_paid(State#state.promise),
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

% exit // any reason
handle_info({'EXIT', Pid, Reason}, State) ->
  % i could do 'countdown' and mark_promise_as_paid here, couldn't i?
  #promise{uuid = PromiseUUID} = State#state.promise,
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // EXIT: ~w ~w~n", [PromiseUUID, Pid, Reason]),
  {noreply, State};
  
% down
handle_info({'DOWN',  Ref, Type, Pid, Info}, State) ->
  #promise{uuid = PromiseUUID} = State#state.promise,
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // EXIT: ~w ~w ~w ~w~n", [PromiseUUID, Ref, Type, Pid, Info]),
  {noreply, State};
  
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.

% no problem, that's ok
terminate(normal, State) ->
  #promise{uuid = PromiseUUID} = State#state.promise,
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // Terminate: normal~n", [PromiseUUID]),
  terminated;

% handle_info generic fallback (ignore) // any reason, i.e: cameron_workflow_sup:stop_child
terminate(Reason, State) ->
  #promise{uuid = PromiseUUID} = State#state.promise,
  io:format("[cameron_workflow_handler] PromiseUUID: ~s // Terminate: ~w~n", [PromiseUUID, Reason]),
  terminate.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

work(Index, #step_input{promise = _Promise,
                        name    = _Name,
                        url     = URL,
                        payload = _Payload,
                        pname   = _Pname} = StepInput) ->

  % {ok, {{"HTTP/1.1", 200, "OK"},
  %       [_, _, _, _, _, _, _, _, _, _, _],
  %       Result}} = http_helper:http_get(URL),

  case http_helper:http_get(URL) of
    {ok, {{"HTTP/1.1", 200, _}, _, Output}} ->
      StepOutput = #step_output{step_input = StepInput, output = Output},
      notify_paid(Index, StepOutput);
    {ok, {{"HTTP/1.1", _, _}, _, Output}} ->
      StepOutput = #step_output{step_input = StepInput, output = Output},
      notify_error(Index, StepOutput);
    {error, {connect_failed, emfile}} ->
      StepOutput = #step_output{step_input = StepInput, output = "{connect_failed, emfile}"},
      notify_error(Index, StepOutput);
    {error, _Reason} ->
      StepOutput = #step_output{step_input = StepInput, output = "unknown_error"},
      notify_error(Index, StepOutput)
  end,
  
  ok.
  
notify(What, {Index, #step_output{step_input = StepInput} = StepOutput}) ->
  Promise = StepInput#step_input.promise,

  Pname = ?pname(Promise#promise.uuid),
  ok = gen_server:cast(Pname, {What, Index, StepOutput}).

notify_paid(Index, #step_output{} = StepOutput) ->
  notify(notify_paid, {Index, StepOutput}).

notify_error(Index, #step_output{} = StepOutput) ->
  notify(notify_error, {Index, StepOutput}).
  