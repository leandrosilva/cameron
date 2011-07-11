%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Startup module for the Cameron application.

-module(cameron).
-author('Leandro Silva <leandrodoze@gmail.com>').

% admin api
-export([start/0, stop/0, upgrade/0]).
% public api
-export([get_version/0, get_basedir/0, get_web_server_config/0, get_redis_server_config/0]).
-export([get_workflows_config/0, get_workflow/1]).

%%
%% Admin API --------------------------------------------------------------------------------------
%%
        
%% @spec start() -> ok
%% @doc Start the cameron server.
start() ->
  cameron_deps:ensure(),
  application:start(?MODULE).

%% @spec stop() -> ok
%% @doc Stop the cameron server.
stop() ->
  _Res = application:stop(?MODULE).

%% @spec upgrade() -> ok
%% @doc Upgrade the cameron server code.
upgrade() ->
  upgrade_code(),
  upgrade_app().

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec get_version() -> string()
%% @doc The Cameron version.
get_version() ->
  get_key(vsn).

%% @spec get_basedir() -> string()
%% @doc Return the application directory for the cameron server.
get_basedir() ->
  {file, Here} = code:is_loaded(?MODULE),
  filename:dirname(filename:dirname(Here)).

%% @spec get_web_server_config() -> string()
%% @doc The Cameron web server configuration.
get_web_server_config() ->
  get_env(web_server).

%% @spec get_redis_server_config() -> string()
%% @doc Redis server configuration.
get_redis_server_config() ->
  get_env(redis_server).

%% @spec get_workflows_config() -> {workflows, [{workflow, {name, Name},
%%                                                         {start_point_url, URL}}]}
%% @doc Workflows configuration.
get_workflows_config() ->
  {ok, [[WorkflowsConfigFile]]} = init:get_argument(workflows),
  {ok, [{workflows, WorkflowsConfig}]} = file:consult(WorkflowsConfigFile),
  
  WorkflowsConfig.

%% @spec get_workflow(Name) -> {start_point_url, URL} | undefined
%% @doc Get a workflow configuration by name.
get_workflow(Name) ->
  proplists:get_value(Name, get_workflows_config(), undefined).
  
%%
%% Internal Functions -----------------------------------------------------------------------------
%%

%% @spec get_key(Name) -> Val | undefined
%% @doc Get a attribute info of this application.
get_key(Name) ->
  case application:get_key(?MODULE, Name) of
    {ok, Value} ->
      Value;
    undefined ->
      undefined
  end.

%% @spec get_env(Name) -> Val | undefined
%% @doc Get a configuration parameter of this application.
get_env(Name) ->
  case application:get_env(?MODULE, Name) of
    {ok, Value} ->
      Value;
    undefined ->
      undefined
  end.

%% @spec upgrade_code() -> [{module, Module}]
%% @doc Upgrade the cameron server code.
upgrade_code() ->
  {ok, LoadedModules} = application:get_key(cameron, modules),
  
  [code:purge(Module) || Module <- LoadedModules],
  [code:load_file(Module) || Module <- LoadedModules].
  
%% @spec upgrade_app() -> [{module, Module}]
%% @doc Upgrade the cameron server application.
upgrade_app() ->
  {ok, {AppName, _}} = application:get_key(?MODULE, mod),
  AppName:upgrade().
  