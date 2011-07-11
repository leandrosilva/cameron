%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The misultin-based web handler module for handle HTTP requests at Cameron web API.
%%      It means:
%%
%%      http://{cameron_host}:{port}/api

-module(cameron_web_api).
-author('Leandro Silva <leandrodoze@gmail.com>').

% misultin web handler callbacks
-export([handle_http/3]).

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
  Req:ok([{"Content-Type", "text/plain"}], "Cameron Workflow System // Web API");

% handle a GET on /api/workflow/{name}/start
handle_http('POST', ["api", "workflow", WorkflowName, "start"], Req) ->
  Body = get_body(Req),
  WorkflowRequest = build_request(WorkflowName, Body),
  
  {ok, Ticket} = cameron_dispatcher:notify_incoming_request(WorkflowRequest),
  
  Req:respond(201, [{"Content-Type", "application/json"},
                    {"Location", ["http://localhost:8080/api/workflow/diagnostic/ticket/", Ticket]}],
                   "{\"payload\":\"~s\"}", [Body]);

% handle the 404 page not found
handle_http(_, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

get_body(Req) ->
  {req, _, _, _, _, _, _, _, _, _, _, _, _, Body} = Req:raw(),
  binary_to_list(Body).
  
build_request(WorkflowName, Body) ->
  Struct = struct:from_json(Body),
  
  % Key = "key_" ++ cameron_ticket:uuid(),
  Key = binary_to_list(struct:get_value(<<"key">>, Struct)),
  Data = binary_to_list(struct:get_value(<<"data">>, Struct)),
  From = binary_to_list(struct:get_value(<<"from">>, Struct)),
  
  #workflow_request{workflow_name = WorkflowName, key = Key, data = Data, from = From}.
