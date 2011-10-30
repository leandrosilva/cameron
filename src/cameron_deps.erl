%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%      loading path, and locate resources relative
%%      to this application's path.

-module(cameron_deps).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([ensure/0, ensure/1]).
-export([get_base_dir/0, get_base_dir/1]).
-export([local_path/1, local_path/2]).
-export([deps_on_path/0, new_siblings/1]).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec ensure() -> ok
%% @doc Ensure that the ebin and include paths for dependencies of
%%      this application are on the code path. Equivalent to
%%      ensure(?MODULE).
ensure() ->
  ensure(?MODULE).

%% @spec ensure(Module) -> ok
%% @doc Ensure that all ebin and include paths for dependencies
%%      of the application for Module are on the code path.
ensure(Module) ->
  code:add_paths(new_siblings(Module)),
  code:clash(),
  ok.

%% @spec get_base_dir(Module) -> string()
%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).

%% @spec get_base_dir() -> string()
%% @doc Return the application directory for this application. Equivalent to
%%      get_base_dir(?MODULE).
get_base_dir() ->
  get_base_dir(?MODULE).

%% @spec local_path(Components) -> string()
%% @doc Return an application-relative directory for this application.
%%      Equivalent to local_path(Components, ?MODULE).
local_path(Components) ->
  local_path(Components, ?MODULE).

%% @spec local_path([string()], Module) -> string()
%% @doc Return an application-relative directory from Module's application.
local_path(Components, Module) ->
  filename:join([get_base_dir(Module) | Components]).

%% @spec deps_on_path() -> [ProjNameAndVers]
%% @doc List of project dependencies on the path.
deps_on_path() ->
  F = fun (X, Acc) ->
        ProjDir = filename:dirname(X),
        case {filename:basename(X),
              filename:basename(filename:dirname(ProjDir))} of
            {"ebin", "deps"} ->
                [filename:basename(ProjDir) | Acc];
            _ ->
                Acc
        end
      end,
  ordsets:from_list(lists:foldl(F, [], code:get_path())).

%% @spec new_siblings(Module) -> [Dir]
%% @doc Find new siblings paths relative to Module that aren't already on the
%%      code path.
new_siblings(Module) ->
  Existing = deps_on_path(),
  SiblingEbin = filelib:wildcard(local_path(["deps", "*", "ebin"], Module)),
  Siblings = [filename:dirname(X) || X <- SiblingEbin,
                         ordsets:is_element(
                           filename:basename(filename:dirname(X)),
                           Existing) =:= false],
  lists:filter(fun filelib:is_dir/1,
               lists:append([[filename:join([X, "ebin"]),
                              filename:join([X, "include"])] ||
                                X <- Siblings])).
