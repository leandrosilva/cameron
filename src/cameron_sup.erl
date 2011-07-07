%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Top-level supervisor for the Cameron application.

-module(cameron_sup).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(supervisor).

% admin api
-export([start_link/0, upgrade/0]).
% supervisor callback
-export([init/1]).

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
  supervisor_helper:upgrade(?MODULE).

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
  Dispatcher = {cameron_dispatcher, {cameron_dispatcher, start_link, []},
                                    permanent, 5000, worker, dynamic},

  WorkerSup = {cameron_worker_sup, {cameron_worker_sup, start_link, []},
                                   permanent, 5000, supervisor, dynamic},

  WebServerConfig = cameron:get_web_server_config(),
  WebServer = {cameron_web_server, {cameron_web_server, start_link, [WebServerConfig]},
                                   permanent, 5000, worker, dynamic},

  RedoConfig = cameron:get_redis_server_config(),
  Redo = {cameron_redo, {redo, start_link, [cameron_redo, RedoConfig]},
                        permanent, 5000, worker, dynamic},

  {ok, {{one_for_one, 10, 10}, [Dispatcher, WorkerSup, WebServer, Redo]}}.
