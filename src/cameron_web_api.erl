%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The misultin-based web handler module for handle HTTP requests and WebSocket at Cameron
%%      web API. It means:
%%
%%      http://{cameron_host}:{port}/api

-module(cameron_web_api).
-author('Leandro Silva <leandrodoze@gmail.com>').

% misultin web handler callbacks
-export([handle_http/3, handle_websocket/2]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("include/cameron.hrl").

%%
%% Misultin-based Callbacks for cameron_web_api ---------------------------------------------------
%%

% --- HTTP Routes to support handle_http callback -------------------------------------------------

% handle a GET on /api
handle_http('GET', ["api"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "Cameron Diagnostic System // Web API");

% handle a GET on /api/diagnostic/ask
handle_http('POST', ["api", "diagnostic", "ask"], Req) ->
  Body = get_body(Req),
  Payload = build_payload(Body),
  
  {ok, Ticket} = cameron_dispatcher:dispatch(Payload),
  
  Req:respond(201, [{"Content-Type", "application/json"},
                    {"Location", ["http://localhost:8080/api/diagnostics/ticket/", Ticket]}],
                   "{\"payload\":\"~s\"}", [Body]);

% handle the 404 page not found
handle_http(_, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

% --- WebSockets Routes to support handle_websocket callback --------------------------------------

% handle /api/diagnostic PATH
handle_websocket(["api", "diagnostic", CustomerId, "stream"], Ws) ->
  case cameron_foo:bar(CustomerId) of
    yes ->
      receive
        {browser, Data} ->
          Ws:send(["[CustomerId = ", CustomerId, "] send: ", Data]),
          handle_websocket(["diagnostic", CustomerId, "stream"], Ws);
        closed ->
          closed;
        _Ignore ->
          handle_websocket(["diagnostic", CustomerId, "stream"], Ws)
      after 5000 ->
        Ws:send(["[CustomerId = ", CustomerId, "] server pushing!"]),
        handle_websocket(["diagnostic", CustomerId, "stream"], Ws)
      end;
    no ->
      Ws:send(["[CustomerId = ", CustomerId, "] no, no, no!"])
  end;

handle_websocket(_, _Ws) ->
  ok.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

get_body(Req) ->
  {req, _, _, _, _, _, _, _, _, _, _, _, _, Body} = Req:raw(),
  binary_to_list(Body).
  
build_payload(Body) ->
  Struct = struct:from_json(Body),
  
  % CustomerId = binary_to_list(struct:get_value(<<"customer_id">>, Struct)),
  CustomerId = "customer_" ++ cameron_ticket:uuid(),
  From = binary_to_list(struct:get_value(<<"from_id">>, Struct)),
  
  #diagnostic_request{customer_id = CustomerId, from_id = From}.
