%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The main gen_server, the "ear" of this workflow system which kind of subscribes to
%%      "incoming queue" in order to be notifyed of every "request for diagnostic" and dispatch
%%      it to a new cameron_worker gen_server.

-module(cameron_dispatcher).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/0, stop/0]).
% public api
-export([notify_incoming_request/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {nothing_yet = yes}).

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

%% @spec notify_incoming_request(WorkflowRequest) -> {ok, Ticket} | {error, Reason}
%% @doc It triggers an async dispatch of a resquest to run a workflow.
notify_incoming_request(#workflow_request{} = WorkflowRequest) ->
  io:format("~n~n--- [cameron_dispatcher] incoming workflow request~n"),
  
  {ok, Ticket} = cameron_ticket:create_new(WorkflowRequest),
  
  ok = gen_server:cast(?MODULE, {notify_incoming_request, WorkflowRequest}),
  
  {ok, Ticket}.

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

% dispatches incoming request
handle_cast({notify_incoming_request, WorkflowRequest}, State) ->
  io:format("--- [cameron_dispatcher] dispatching an incoming request~n"),
  
  {ok, Ticket} = cameron_ticket:take_next(WorkflowRequest#workflow_request.workflow_name),

  io:format("--- [cameron_dispatcher] dispatching an incoming request // Ticket: ~w~n", [Ticket]),
  
  ok = cameron_worker:spawn_new(Ticket),
  
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
