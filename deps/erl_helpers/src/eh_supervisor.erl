%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Utility functions to supervisor modules.

-module(eh_supervisor).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([upgrade/1]).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec upgrade(Supervisor) -> ok
%% @doc Remover and add processes if necessary.
upgrade(Supervisor) ->
  {ok, {_, Specs}} = Supervisor:init([]),

  Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(Supervisor)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  
  % kill children processes that doesn't exists anymore
  % I mean, they exist in the Old spec but no longer in the New spec
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
              supervisor:terminate_child(Supervisor, Id),
              supervisor:delete_child(Supervisor, Id),
              ok
            end, ok, Kill),

  [supervisor:start_child(Supervisor, Spec) || Spec <- Specs],
  ok.
