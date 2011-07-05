%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for roomerl_web.

-module(roomerl_web_server_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing roomerl_web module ------------------------------------------------------------------
%%
describe_test_() ->
  {"roomerl_web_server",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"having customizable configuration parameters",
        [
          {"should have get_host() function",
           fun should_have_get_host_function/0},
             
          {"should have get_port() function",
           fun should_have_get_port_function/0},

          {"should have get_backlog() function",
           fun should_have_get_backlog_function/0},

          {"should have get_docroot() function",
           fun should_have_get_docroot_function/0}
        ]},

      {"after all tests",
        {setup, fun after_all/0, []}}
    ]}.

%%
%% Setup ------------------------------------------------------------------------------------------
%%

before_all() ->
  application:start(inets),
  ok.

after_all() ->
  application:stop(inets),
  ok.

%%
%% Scenary: having customizable configuration parameters ------------------------------------------
%%

should_have_get_host_function() ->
  ?assertMatch("127.0.0.1", roomerl_web_server:get_host()).
   
should_have_get_port_function() ->
  ?assertMatch(8008, roomerl_web_server:get_port()).

should_have_get_backlog_function() ->
  ?assertMatch(111, roomerl_web_server:get_backlog()).
  
should_have_get_docroot_function() ->
  ?assertMatch("priv/www", roomerl_web_server:get_docroot()).
