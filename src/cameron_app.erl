%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Callbacks for the Cameron application.

-module(cameron_app).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(application).

% admin api
-export([start/2, stop/1, upgrade/0]).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start(_Type, _StartArgs) -> {ok, SupervisorPid} | ignore | {error, Error}
%% @doc application start callback for cameron.
start(_Type, _StartArgs) ->
  cameron_deps:ensure(),
  inets:start(),
  cameron_sup:start_link().

%% @spec stop(_State) -> ok
%% @doc application stop callback for cameron.
stop(_State) ->
  inets:stop(),
  ok.

%% @spec upgrade() -> ok
%% @doc Upgrade the Cameron application code.
upgrade() ->
  cameron_sup:upgrade().
  