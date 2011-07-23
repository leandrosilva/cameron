%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Supervisor for the workers of the Cameron application.

-module(cameron_workflow_sup).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(supervisor).

% admin api
-export([start_link/0, upgrade/0]).
% public api
-export([start_child/1, stop_child/1, which_children/0]).
% supervisor callback
-export([init/1]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link() -> Result = {ok, Pid} | ignore | {error, Error}
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Remove and add processes if necessary.
upgrade() ->
  supervisor_utility:upgrade(?MODULE).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec start_child(Promise) -> {ok, ChildPid} | {ok, ChildPid, Info} | {error, Error}
%% @dynamic Start a cameron_workflow_{Promise} process to diagnostic a Promise given.
start_child(#promise{} = Promise) ->
  Pname = pname(Promise),

  WorkflowSpec = {Pname, {cameron_workflow_handler, start_link, [Pname, Promise]}, temporary, 5000, worker, dynamic},
  supervisor:start_child(cameron_workflow_sup, WorkflowSpec).

%% @spec stop_child(Promise) -> ok | {error, Error}
%% @dynamic Stop a cameron_workflow_{Promise}.
stop_child(#promise{} = Promise) ->
  stop_child(pname(Promise));

stop_child(PromiseUUID) when is_list(PromiseUUID) ->
  stop_child(pname(PromiseUUID));

stop_child(Pname) when is_atom(Pname) ->
  supervisor:terminate_child(cameron_workflow_sup, Pname),
  supervisor:delete_child(cameron_workflow_sup, Pname).
  
%% @spec which_children() -> [ChildSpec] | {error, Error}
%% @dynamic List of children workers.
which_children() ->
  supervisor:which_children(cameron_workflow_sup).
  
%%
%% Supervisor Callback ----------------------------------------------------------------------------
%%

%% @spec init([]) -> SupervisorTree = {ok, {SupervisorSpec, [ChildSpec]}} | ignore
%%
%% Types:
%%
%%     SupervisorSpec = {RestartStrategy, AllowedRestarts, MaxSeconds}
%%     ChildSpec = {Id, {Module, Function, Arguments}, Restart, Shutdown, Type, ModuleList}
%%
%% @doc supervisor callback.
init([]) ->
  WorkflowPersistence = {cameron_workflow_persistence, {cameron_workflow_persistence, start_link, []},
                                                       permanent, 5000, worker, dynamic},

  WorkflowCatalogConfig = cameron:get_workflows_config(),
  WorkflowCatalog = {cameron_workflow_catalog, {cameron_workflow_catalog, start_link, [WorkflowCatalogConfig]},
                                                permanent, 5000, worker, dynamic},

  WorkflowDispatcher = {cameron_workflow_dispatcher, {cameron_workflow_dispatcher, start_link, []},
                                                      permanent, 5000, worker, dynamic},
                                                      
  {ok, {{one_for_one, 10, 10}, [WorkflowPersistence, WorkflowCatalog, WorkflowDispatcher]}}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

pname(#promise{uuid = PromiseUUID}) ->
  pname(PromiseUUID);

pname(PromiseUUID) when is_list(PromiseUUID) ->
  ?pname(PromiseUUID).
  