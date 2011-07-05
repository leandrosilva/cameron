%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for roomerl_rooms_web_handler.

-module(roomerl_rooms_web_handler_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing roomerl_rooms_web_handler module ------------------------------------------------------------
%%
describe_test_() ->
  {"roomerl_rooms_web_handler",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"when in HTTP mode",
        [
          {"should accept GET on / to show the home page",
            fun should_accept_get_to_show_the_home_page/0},

          {"should accept GET on /rooms to show the rooms page",
            fun should_accept_get_to_show_the_rooms_page/0},

          {"should accept GET on /rooms/{RoomId} to show the profile page of a room",
            fun should_accept_get_to_show_the_profile_page_of_a_room/0},

          {"should accept GET on /rooms/{RoomId}/open to open a room",
            fun should_accept_get_to_open_a_room/0},

            {"should accept GET on /rooms/{RoomId}/close to close a room",
              fun should_accept_get_to_close_a_room/0},

          {"should accept GET on /rooms/{RoomId}/enter to enter in a room",
            fun should_accept_get_to_enter_in_a_room/0},

          {"should accept GET on /rooms/{RoomId}/users to show the users page of a room",
            fun should_accept_get_to_show_the_users_page_of_a_room/0}
        ]},

        {"when in WebSocket mode",
          [
            {"should accept connections on /rooms/{RoomId}",
              fun should_accept_connections_in_a_room/0}
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
%% Scenary: when in HTTP mode ---------------------------------------------------------------------
%%

-define(BASE_URL, "http://localhost:8008").

should_accept_get_to_show_the_home_page() ->
  HttpResponse = http_get("/"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "12"},
                      {"content-type", "text/plain"}],
                     "qsin:roomerl"}}, HttpResponse).
  
should_accept_get_to_show_the_rooms_page() ->
  HttpResponse = http_get("/rooms"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "15"},
                      {"content-type", "text/plain"}],
                     "The Rooms page."}}, HttpResponse).
  
should_accept_get_to_show_the_profile_page_of_a_room() ->
  HttpResponse = http_get("/rooms/123"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "26"},
                      {"content-type", "text/plain"}],
                     "This is the 123 room page."}}, HttpResponse).

should_accept_get_to_open_a_room() ->
  HttpResponse = http_get("/rooms/123/open"),

  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "17"},
                      {"content-type", "text/plain"}],
                     "Opening room 123."}}, HttpResponse).

should_accept_get_to_close_a_room() ->
  HttpResponse = http_get("/rooms/123/close"),

  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "17"},
                      {"content-type", "text/plain"}],
                     "Closing room 123."}}, HttpResponse).
  
should_accept_get_to_enter_in_a_room() ->
  HttpResponse = http_get("/rooms/123/enter"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "1595"},
                      {"content-type", "text/html"}],
                     _HtmlBody}}, HttpResponse).
  
should_accept_get_to_show_the_users_page_of_a_room() ->
  HttpResponse = http_get("/rooms/123/users"),
  
  ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"},
                     [{"connection", "Keep-Alive"},
                      {"content-length", "25"},
                      {"content-type", "text/plain"}],
                     "This is 123's users page."}}, HttpResponse).

%%
%% Scenary: when in WebSocket mode ----------------------------------------------------------------
%%

should_accept_connections_in_a_room() ->
  [{host, Host}, {port, Port}, _, _] = roomerl:get_web_server_config(),
   
  {ok, {room, "123", roomerl_rooms_123}} = roomerl_admin:open_room("123"),
  started = websocket_client_impl:start(Host, Port, "/rooms/123/student/1"),
  
  {sent, "message 1"} = websocket_client_impl:send("message 1"),
  wait_one_seconds(),
  
  {sent, "message 2"} = websocket_client_impl:send("message 2"),
  wait_one_seconds(),
  
  closed = websocket_client_impl:close(),
  wait_one_seconds(),
  
  ?assertMatch([{send, "message 1"},
                {onopen, ok},
                {onmessage, "[RoomId = 123, StudentId = 1] received 'message 1'"},
                {send, "message 2"},
                {onmessage, "[RoomId = 123, StudentId = 1] received 'message 2'"},
                {close, ok},
                {onclose, ok}], websocket_client_impl:get_log()).
  
%%
%% Helper functions -------------------------------------------------------------------------------
%%

http_get(Uri) ->
  http_request(get, ?BASE_URL ++ Uri).

http_request(HttpMethod, Uri) ->
  httpc:request(HttpMethod, {Uri, []}, [], []).

wait_one_seconds() ->
  timer:sleep(1000).
