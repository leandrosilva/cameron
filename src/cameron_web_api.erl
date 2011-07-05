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
%% Misultin-based Callbacks for cameron_webcare -------------------------------------------------------
%%

% --- HTTP Routes to support handle_http callback -------------------------------------------------

% handle a GET on /api
handle_http('GET', ["api"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "Cameron Diagnostic System // Web API");

% handle a GET on /api/diagnostic/ask
handle_http('POST', ["api", "diagnostic", "ask"], Req) ->
  Body = get_body(Req),
  Payload = get_payload(Body),
  
  ok = cameron_dispatcher:dispatch(Payload),
  
  Req:respond(201, [{"Content-Type", "application/json"}], "{\"payload\":\"~s\"}", [Body]);

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
%% Internal API -----------------------------------------------------------------------------------
%%

get_body(Req) ->
  {req, _, _, _, _, _, _, _, _, _, _, _, _, Body} = Req:raw(),
  binary_to_list(Body).
  
get_payload(Body) ->
  Struct = struct:from_json(Body),
  
  Customer = {customer, {binary_to_atom(struct:get_value({<<"customer">>, <<"type_id">>}, Struct), utf8),
                         binary_to_list(struct:get_value({<<"customer">>, <<"value_id">>}, Struct))}},
  From = {from, binary_to_list(struct:get_value(<<"from">>, Struct))},
  
  {request_for_diagnostic, Customer, From}.
  