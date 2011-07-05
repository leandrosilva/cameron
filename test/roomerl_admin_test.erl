%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for roomerl_admin.

-module(roomerl_admin_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing rooms module ------------------------------------------------------------------------
%%
describe_test_() ->
  {"roomerl_admin",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"as a room manager",
        [
          {"should open a room given",
            fun should_open_a_room_given/0},

          {"should close a room given",
            fun should_close_a_room_given/0},
          
          {"should get a room given",
            fun should_get_a_room_given/0},

          {"should list all open rooms",
            fun should_list_all_open_rooms/0},

          {"should know whether a room is open or no",
            fun should_know_whether_a_room_is_open_or_no/0},

          {"should close all open rooms",
            fun should_close_all_open_rooms/0}
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
%% Scenary: as a room manager ---------------------------------------------------------------------
%%

should_open_a_room_given() ->
  ensure_that_all_rooms_is_close(),
  
  ?assertMatch({ok, {room, "123", roomerl_rooms_123}}, roomerl_admin:open_room("123")),
  ?assertMatch([{room, "123", roomerl_rooms_123}], roomerl_admin:get_open_rooms()).

should_close_a_room_given() ->
  ensure_that_all_rooms_is_close(),
  
  ?assertMatch({ok, {room, "123", roomerl_rooms_123}}, roomerl_admin:open_room("123")),
  ?assertMatch({ok, {room, "456", roomerl_rooms_456}}, roomerl_admin:open_room("456")),
  
  wait_a_moment(),
  
  ?assertMatch({ok, {room, "123", roomerl_rooms_123}}, roomerl_admin:close_room("123")),
  ?assertMatch([{room, "456", roomerl_rooms_456}], roomerl_admin:get_open_rooms()).

should_get_a_room_given() ->
  ensure_that_all_rooms_is_close(),

  ?assertMatch(unknow, roomerl_admin:get_room("123")),
  ?assertMatch({ok, {room, "123", roomerl_rooms_123}}, roomerl_admin:open_room("123")),
  ?assertMatch({room, "123", roomerl_rooms_123}, roomerl_admin:get_room("123")).

should_list_all_open_rooms() ->
  ensure_that_all_rooms_is_close(),
  
  roomerl_admin:open_room("123"),
  roomerl_admin:open_room("456"),
  roomerl_admin:open_room("789"),
  
  ?assertMatch([{room, "123", roomerl_rooms_123},
                {room, "456", roomerl_rooms_456},
                {room, "789", roomerl_rooms_789}], roomerl_admin:get_open_rooms()).

should_know_whether_a_room_is_open_or_no() ->
  ensure_that_all_rooms_is_close(),

  roomerl_admin:open_room("123"),
  roomerl_admin:open_room("456"),
  roomerl_admin:open_room("789"),

  ?assertMatch(yes, roomerl_admin:is_open_room("123")),
  ?assertMatch(yes, roomerl_admin:is_open_room("456")),
  ?assertMatch(yes, roomerl_admin:is_open_room("789")),
  ?assertMatch(no, roomerl_admin:is_open_room("000")).
    
should_close_all_open_rooms() ->
  ensure_that_all_rooms_is_close(),

  roomerl_admin:open_room("123"),
  roomerl_admin:open_room("456"),
  roomerl_admin:open_room("789"),

  ?assertMatch([], roomerl_admin:close_all_rooms()),
  ?assertMatch([], roomerl_admin:get_open_rooms()).
  
%%
%% Helper functions -------------------------------------------------------------------------------
%%

ensure_that_all_rooms_is_close() ->
  ?assertMatch([], roomerl_admin:close_all_rooms()),
  ?assertMatch([], roomerl_admin:get_open_rooms()),
  
  wait_a_moment(),
  ok.

wait_a_moment() ->
  timer:sleep(1).
  