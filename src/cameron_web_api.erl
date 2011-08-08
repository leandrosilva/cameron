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

-include("cameron.hrl").
-include("misultin.hrl").

%%
%% Misultin-based Callbacks for cameron_web_api ---------------------------------------------------
%%

% --- HTTP Routes to support handle_http callback -------------------------------------------------

% handle a GET on /api
handle_http('GET', ["api"], HttpRequest) ->
  HttpRequest:ok([{"Content-Type", "text/plain"}], "Cameron Workflow Engine // Web API");

handle_http('POST', ["api", "process", ProcessName, "start"], HttpRequest) ->
  Payload = get_request_payload(HttpRequest),

  case cameron_process_catalog:lookup(ProcessName) of
    undefined ->
      HttpRequest:respond(404, [{"Content-Type", "application/json"}],
                                 "{\"payload\":\"~s\"}", [Payload]);
    Process ->
      {Key, Data, Requestor} = cameron_protocol:parse_request_payload(Payload),
      {ok, UUID} = cameron_job_scheduler:schedule(Process, {Key, Data, Requestor}),
      
      HttpRequest:respond(201, [{"Content-Type", "application/json"},
                                {"Location", ["http://localhost:8080/api/process/", ProcessName,
                                              "/key/", Key,
                                              "/job/", UUID]}],
                               "{\"payload\":\"~s\"}", [Payload])
  end;

% handle a GET on /api/process/{name}/key/{key}/job/{uuid}
handle_http('GET', ["api", "process", ProcessName, "key", Key, "job", UUID], HttpRequest) ->
  case cameron_job_data:get_job_data(ProcessName, Key, UUID) of
    undefined ->
      HttpRequest:respond(404, [{"Content-Type", "text/plain"}], "Job not found.");
    {ok, Data} ->
      Payload = cameron_protocol:build_response_payload({ProcessName, Key, UUID}, Data),

      HttpRequest:respond(200, [{"Content-Type", "application/json"}], "~s", [Payload])
  end;    

% handle the 404 page not found
handle_http(_, _, HttpRequest) ->
  HttpRequest:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

% --- helpers to POST on /api/process/{name}/start ------------------------------------------------

get_request_payload(HttpRequest) ->
  #req{body = Body} = HttpRequest:raw(),
  binary_to_list(Body).
