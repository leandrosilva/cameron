%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The misultin-based web handler module for handle HTTP requests at Cameron web API.

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
handle_http('GET', ["api"], HttpRequest) ->
  HttpRequest:ok([{"Content-Type", "text/plain"}], "Cameron Workflow System // Web API");

% handle a GET on /api/workflow/{name}/start
handle_http('POST', ["api", "workflow", WorkflowName, "start"], HttpRequest) ->
  Payload = get_request_payload(HttpRequest),

  case cameron_workflow:lookup(WorkflowName) of
    undefined ->
      HttpRequest:respond(404, [{"Content-Type", "application/json"}],
                       "{\"payload\":\"~s\"}", [Payload]);
    Workflow ->
      Request = build_request(Workflow, Payload),
  
      {ok, Promise} = cameron_dispatcher:dispatch_request(Request),
  
      HttpRequest:respond(201, [{"Content-Type", "application/json"},
                        {"Location", ["http://localhost:8080/api/workflow/", WorkflowName,
                                      "/key/", Request#request.key,
                                      "/promise/", Promise#promise.uuid]}],
                       "{\"payload\":\"~s\"}", [Payload])
  end;

% handle the 404 page not found
handle_http(_, _, HttpRequest) ->
  HttpRequest:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

get_request_payload(HttpRequest) ->
  {req, _, _, _, _, _, _, _, _, _, _, _, _, Body} = HttpRequest:raw(),
  binary_to_list(Body).

build_request(Workflow, Payload) ->
  Struct = struct:from_json(Payload),
  
  Key = binary_to_list(struct:get_value(<<"key">>, Struct)),
  Data = binary_to_list(struct:get_value(<<"data">>, Struct)),
  From = binary_to_list(struct:get_value(<<"from">>, Struct)),
  
  #request{workflow = Workflow,
           key      = Key,
           data     = Data,
           from     = From}.
