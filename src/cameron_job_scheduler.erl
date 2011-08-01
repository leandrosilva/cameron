%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Generic server responsable to create process instances (a.k.a. jobs), enqueue it, and
%%      dispatch to cameron_job_runner, whose which runs it.

-module(cameron_job_scheduler).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/0, stop/0]).
% public api
-export([schedule/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

-record(state, {}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Start cameron_job_scheduler generic server.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec schedule(Process, {Key, Data, Requestor}) -> {ok, NewJobUUID} | {error, Reason}
%% @doc It triggers an async schedule of a resquest to create a new job and enqueue it to run.
schedule(Process, {Key, Data, Requestor}) ->
  {ok, NewJob} = cameron_job_data:create_new_job(Process, {Key, Data, Requestor}),
  ok = gen_server:cast(?MODULE, {dispatch_new_job, NewJob}),
  {ok, NewJob#job.uuid}.

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

%% @spec handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling cast messages.

% schedules a new job to be done
handle_cast({dispatch_new_job, NewJob}, State) ->
  ok = cameron_job_runner:run_job(NewJob),
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
