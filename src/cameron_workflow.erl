%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The worker gen_server, the responsable to make diagnostic.

-module(cameron_workflow).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/1]).
% public api
-export([lookup/1, accept_request/1, work/2]).
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
%% @doc Start a cameron_workflow server.
start_link(Promise) ->
  Pname = cameron_workflow_sup:which_child(Promise),
  gen_server:start_link({local, Pname}, ?MODULE, [Promise], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop(Promise) ->
  Pname = pname_for(Promise),
  gen_server:cast(Pname, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec lookup(Name) -> {Name, {start_url, URL}} | undefined
%% @doc Get a workflow configuration by name.
lookup(Name) when is_atom(Name) ->
  Spec = proplists:get_value(Name, cameron:get_workflows_config(), undefined),
  
  case Spec of 
    undefined        -> undefined;
    {start_url, URL} -> #workflow{name = Name, start_url = URL}
  end;
  
lookup(Name) when is_list(Name) ->
  lookup(list_to_atom(Name)).

%% @spec accept_request(Request) -> {ok, Promise} | {error, Reason}
%% @doc It triggers an async dispatch of a resquest to run a workflow an pay a promise.
accept_request(#request{} = Request) ->
  io:format("~n~n--- [cameron_workflow] accepting incoming workflow request~n"),
  
  {ok, Promise} = cameron_workflow_keeper:accept_new_request(Request),
  
  case pay_it(Promise) of
    ok              -> {ok, Promise};
    {error, Reason} -> {error, Reason}
  end.

%% @spec pay_it(Promise) -> ok
%% @doc Create a new process, child of cameron_workflow_sup, and then make a complete diagnostic (in
%%      parallel, of course) to the promise given.
pay_it(#promise{uuid = PromiseUUID} = Promise) ->
  case cameron_workflow_sup:start_child(PromiseUUID) of
    {ok, _Pid} ->
      Pname = pname_for(PromiseUUID),
      ok = gen_server:cast(Pname, {pay_it, Promise});
    {error, {already_started, _Pid}} ->
      ok
  end,
  ok.

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
  io:format("~n--- [cameron_workflow] paying // Promise: ~w~n", [Promise]),
  
  CloudInput = #step_input{promise = Promise,
                           name    = "cloud_zabbix",
                           url     = "http://localhost:9292/workflow/v0.0.1/cloud/zabbix",
                           payload = "xxx",
                           pname   = pname_for(PromiseUUID)},
  
  spawn(?MODULE, work, [1, CloudInput]),
  
  HostInput = #step_input{promise = Promise,
                          name    = "hosting_zabbix",
                          url     = "http://localhost:9292/workflow/v0.0.1/hosting/zabbix",
                          payload = "yyy",
                          pname   = pname_for(PromiseUUID)},
  
  spawn(?MODULE, work, [2, HostInput]),
  
  SqlServerInput = #step_input{promise = Promise,
                               name    = "sqlserver_zabbix",
                               url     = "http://localhost:9292/workflow/v0.0.1/sqlserver/zabbix",
                               payload = "zzz",
                               pname   = pname_for(PromiseUUID)},
  
  spawn(?MODULE, work, [3, SqlServerInput]),
  
  {noreply, State#state{countdown = 3}};

% notify when a individual diagnostic is done
handle_cast({notify_paid, Index, #step_output{step_input = StepInput} = StepOutput}, State) ->
  io:format("--- [~s] Index: ~w, Pname: ~s // Notified its work is done~n", [StepInput#step_input.name,
                                                                             Index,
                                                                             StepInput#step_input.pname]),
  
  {ok, Promise} = cameron_workflow_keeper:save_promise_payment_progress(StepOutput),

  case State#state.countdown of
    1 ->
      {ok, Promise} = cameron_workflow_keeper:mark_promise_as_paid(Promise),
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
  io:format("~n--- [cameron_workflow] terminating // Promise: ~s, Reason: ~w~n", [State#state.promise_uuid, Reason]),
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

pname_for(PromiseUUID) ->
  cameron_workflow_sup:which_child(PromiseUUID).

work(Index, #step_input{promise = Promise,
                        name    = Name,
                        url     = URL,
                        payload = _Payload,
                        pname   = Pname} = StepInput) ->
  io:format("--- [(~w) cameron_workflow_~s] Index: ~w, Pname: ~s, Name: ~s~n", [self(),
                                                                                Promise#promise.uuid,
                                                                                Index, 
                                                                                Pname, 
                                                                                Name]),

  % {ok, {{"HTTP/1.1", 200, "OK"},
  %       [_, _, _, _, _, _, _, _, _, _, _],
  %       Result}} = http_helper:http_get(URL),

  {ok, {{"HTTP/1.1", 200, _}, _, Output}} = http_helper:http_get(URL),
         
  StepOutput = #step_output{step_input = StepInput, output = Output},

  io:format("--- [(~w) cameron_workflow_~s] Index: ~w, Pname: ~s, Name: ~s // Paid~n", [self(),
                                                                                        Promise#promise.uuid,
                                                                                        Index, 
                                                                                        Pname, 
                                                                                        Name]),
  
  notify_paid(Index, StepOutput),
  ok.
  
notify_paid(Index, #step_output{step_input = StepInput} = StepOutput) ->
  Promise = StepInput#step_input.promise,
  
  Pname = pname_for(Promise#promise.uuid),
  ok = gen_server:cast(Pname, {notify_paid, Index, StepOutput}).
  