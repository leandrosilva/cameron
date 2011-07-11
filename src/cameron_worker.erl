%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The worker gen_server, the responsable to make diagnostic.

-module(cameron_worker).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/1]).
% public api
-export([spawn_new/1, get_name/1, work/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {ticket, countdown}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(Ticket) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start cameron server.
start_link(Ticket) ->
  WorkerName = cameron_worker_sup:which_child(Ticket),
  gen_server:start_link({local, WorkerName}, ?MODULE, [Ticket], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop(Ticket) ->
  WorkerName = get_name(Ticket),
  gen_server:cast(WorkerName, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec spawn_new(Ticket) -> ok
%% @doc Create a new process, child of cameron_worker_sup, and then make a complete diagnostic (in
%%      parallel, of course) to the ticket given.
spawn_new(#workflow_ticket{short_uuid = TicketShortUUID} = Ticket) ->
  case cameron_worker_sup:start_child(TicketShortUUID) of
    {ok, _Pid} ->
      WorkerName = get_name(TicketShortUUID),
      ok = gen_server:cast(WorkerName, {spawn_new, Ticket});
    {error, {already_started, _Pid}} ->
      ok
  end,
  ok.

%% @spec get_name(TicketShortUUID) -> WorkerName
%% @doc Which worker is handling a ticket given.
get_name(TicketShortUUID) ->
  cameron_worker_sup:which_child(TicketShortUUID).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(Ticket) ->
  {ok, #state{ticket = Ticket}}.

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

% wake up to run a workflow
handle_cast({spawn_new, #workflow_ticket{workflow_name = WorkflowName, short_uuid = TicketShortUUID} = Ticket}, State) ->
  io:format("~n--- [cameron_worker] running // Ticket: ~s~n", [Ticket]),
  
  CloudInput = #workflow_step_input{workflow_name     = WorkflowName,
                                    ticket_short_uuid = TicketShortUUID,
                                    name              = "cloud_zabbix",
                                    url               = "http://localhost:9292/workflow/v0.0.1/cloud/zabbix",
                                    payload           = "",
                                    worker_name       = get_name(TicketShortUUID)},
  
  spawn(?MODULE, work, [1, CloudInput]),
  
  HostInput = #workflow_step_input{workflow_name     = WorkflowName,
                                   ticket_short_uuid = TicketShortUUID,
                                   name              = "hosting_zabbix",
                                   url               = "http://localhost:9292/workflow/v0.0.1/hosting/zabbix",
                                   payload           = "",
                                   worker_name       = get_name(TicketShortUUID)},
  
  spawn(?MODULE, work, [2, HostInput]),
  
  SqlServerInput = #workflow_step_input{workflow_name     = WorkflowName,
                                        ticket_short_uuid = TicketShortUUID,
                                        name              = "sqlserver_zabbix",
                                        url               = "http://localhost:9292/workflow/v0.0.1/sqlserver/zabbix",
                                        payload           = "",
                                        worker_name       = get_name(TicketShortUUID)},
  
  spawn(?MODULE, work, [3, SqlServerInput]),
  
  {noreply, State#state{countdown = 3}};

% notify when a individual diagnostic is done
handle_cast({notify_done, Index, #workflow_step_output{workflow_name     = _WorkflowName,
                                                       ticket_short_uuid = _TicketShortUUID,
                                                       name              = Name,
                                                       url               = _URL,
                                                       payload           = _Payload,
                                                       output            = _Output,
                                                       worker_name       = WorkerName}} = StepOutput, State) ->
  io:format("[~s] Index: ~w, WorkerName: ~s // Notified its work is done~n", [Name, Index, WorkerName]),
  
  {ok, Ticket} = cameron_ticket:save_output(StepOutput),

  case State#state.countdown of
    1 ->
      {ok, Ticket} = cameron_ticket:close(Ticket),
      NewState = State#state{countdown = 0},
      % ok = cameron_worker_sup:stop_child(Ticket) % supervisor verifica quem esta done e mata?
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
terminate(_Reason, State) ->
  io:format("~n--- [cameron_worker] terminating // Ticket: ~s~n", [State#state.ticket]),
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

work(Index, #workflow_step_input{workflow_name     = WorkflowName,
                                 ticket_short_uuid = TicketShortUUID,
                                 name              = Name,
                                 url               = URL,
                                 payload           = _Payload,
                                 worker_name       = WorkerName}) ->
  io:format("--- [cameron_worker_~s] Index: ~w, WorkerName: ~s, Name: ~s~n", [TicketShortUUID, Index, WorkerName, Name]),

  % {ok, {{"HTTP/1.1", 200, "OK"},
  %       [_, _, _, _, _, _, _, _, _, _, _],
  %       Result}} = http_helper:http_get(URL),

  {ok, {{"HTTP/1.1", 200, _}, _, Output}} = http_helper:http_get(URL),
         
  io:format("--- [cameron_worker_~s] Index: ~w, WorkerName: ~s, Name: ~s // Done~n", [TicketShortUUID, Index, WorkerName, Name]),

  Output = #workflow_step_output{workflow_name     = WorkflowName,
                                 ticket_short_uuid = TicketShortUUID,
                                 name              = Name,
                                 url               = URL,
                                 payload           = _Payload,
                                 output            = Output,
                                 worker_name       = WorkerName},

  notify_done(Index, Output),
  ok.
  
notify_done(Index, #workflow_step_output{workflow_name     = _WorkflowName,
                                         ticket_short_uuid = TicketShortUUID,
                                         name              = _Name,
                                         url               = _URL,
                                         payload           = _Payload,
                                         output            = _Output,
                                         worker_name       = WorkerName} = StepOutput) ->
  WorkerName = get_name(TicketShortUUID),
  ok = gen_server:cast(WorkerName, {notify_done, Index, StepOutput}).
  