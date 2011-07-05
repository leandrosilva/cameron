%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for roomerl.

-module(roomerl_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing roomerl module ----------------------------------------------------------------------
%%
describe_test_() ->
  {"roomerl",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"having a public API",
        [
          {"should has get_basedir()",
            fun should_has_get_basedir/0},
              
          {"should has get_web_server_config()",
            fun should_has_get_web_server_config/0}
        ]},

      {"after all tests",
        {setup, fun after_all/0, []}}
    ]}.

%%
%% Setup ------------------------------------------------------------------------------------------
%%

before_all() ->
  ok.

after_all() ->
  ok.

%%
%% Scenary: having a public API -------------------------------------------------------------------
%%

should_has_get_basedir() ->
  ?assertMatch("roomerl", lists:last(string:tokens(roomerl:get_basedir(), "/"))).

should_has_get_web_server_config() ->
  ?assertMatch([{host, "127.0.0.1"},
                {port, 8008},
                {backlog, 111},
                {docroot, "priv/www"}], roomerl:get_web_server_config()).
