%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for room.

-module(roomerl_rooms_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing room module -------------------------------------------------------------------------
%%
describe_test_() ->
  {"roomerl_rooms",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"as a room",
        [
          {"should have a name",
            fun should_have_a_name/0},

          {"should know whether is started or no",
            fun should_know_whether_is_started_or_no/0},

          {"should can be started",
            fun should_can_ben_started/0},

          {"should can be stopped",
            fun should_can_be_stopped/0},
          
          {"should say welcome to a user",
            fun should_say_welcome_to_a_user/0},

          {"should say goodbye to a user",
            fun should_say_goodbye_to_a_user/0},

          {"should know whether a user is present or no",
            fun should_know_whether_a_user_is_present_or_no/0},
          
          {"should publish messages",
            fun should_publish_messages/0}
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

should_know_whether_is_started_or_no() ->
  ensure_room_is_stopped("123"),
  
  ?assertMatch(no, roomerl_rooms:is_started("123")),
  ?assertMatch({ok, _Pid}, roomerl_rooms:start_link("123")),
  ?assertMatch(yes, roomerl_rooms:is_started("123")).

should_can_ben_started() ->
  ensure_room_is_stopped("123"),
  
  ?assertMatch({ok, _Pid}, roomerl_rooms:start_link("123")).

should_can_be_stopped() ->
  ensure_room_is_stopped("123"),
  
  ?assertMatch({ok, _Pid}, roomerl_rooms:start_link("123")),
  ?assertMatch(ok, roomerl_rooms:stop("123")).

should_have_a_name() ->
  ?assertMatch(roomerl_rooms_123, roomerl_rooms:get_name("123")).

should_say_welcome_to_a_user() ->
  ?assertMatch(yes, no).

should_say_goodbye_to_a_user() ->
  ?assertMatch(yes, no).

should_know_whether_a_user_is_present_or_no() ->
  ?assertMatch(yes, no).

should_publish_messages() ->
  ?assertMatch(yes, no).
    
%%
%% Helper functions -------------------------------------------------------------------------------
%%

ensure_room_is_stopped(RoomId) ->
  roomerl_rooms:stop(RoomId),
  
  timer:sleep(1),
  ok.
