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

% handle a POST on /api/process/{name}/start
handle_http('POST', ["api", "process", ProcessName, "start"], HttpRequest) ->
  Payload = get_request_payload(HttpRequest),

  case cameron_process_catalog:lookup(ProcessName) of
    undefined ->
      HttpRequest:respond(404, [{"Content-Type", "application/json"}],
                                 "{\"payload\":\"~s\"}", [Payload]);
    Process ->
      {Key, Data, Requestor} = parse_request_payload(Payload),
      {ok, UUID} = cameron_job_scheduler:schedule(Process, {Key, Data, Requestor}),
      
      HttpRequest:respond(201, [{"Content-Type", "application/json"},
                                {"Location", ["http://localhost:8080/api/process/", ProcessName,
                                              "/key/", Key,
                                              "/job/", UUID]}],
                               "{\"payload\":\"~s\"}", [Payload])
  end;

% handle a GET on /api/process/{name}/key/{key}/job/{uuid}
handle_http('GET', ["api", "process", ProcessName, "key", Key, "job", UUID], HttpRequest) ->
  {ok, Data} = cameron_job_data:get_job_data(ProcessName, Key, UUID),
  JsonData = generate_json_response({ProcessName, Key, UUID}, Data),
  
  HttpRequest:respond(200, [{"Content-Type", "application/json"}], "~s", [JsonData]);

% handle the 404 page not found
handle_http(_, _, HttpRequest) ->
  HttpRequest:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

% --- helpers to POST on /api/process/{name}/start ------------------------------------------------

get_request_payload(HttpRequest) ->
  {req, _, _, _, _, _, _, _, _, _, _, _, _, Body} = HttpRequest:raw(),
  binary_to_list(Body).

parse_request_payload(Payload) ->
  Struct = struct:from_json(Payload),
  
  Key = struct:get_value(<<"key">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, list}),
  Requestor = struct:get_value(<<"requestor">>, Struct, {format, list}),
  
  {Key, Data, Requestor}.

% --- helpers to GET on /api/process/{name}/key/{key}/job/{uuid} ----------------------------------

generate_json_response({ProcessName, Key, UUID}, Data) ->
  Struct = {struct, [{<<"process">>,   maybe_helper:maybe_binary(ProcessName)},
                     {<<"uuid">>,      maybe_helper:maybe_binary(UUID)},
                     {<<"key">>,       maybe_helper:maybe_binary(Key)},
                     {<<"requestor">>, get_job_value("requestor", Data)},
                     {<<"status">>,    expand_job_status(Data)},
                     {<<"tasks">>,     expand_job_tasks(Data)}]},
  struct:to_json(Struct).

expand_job_status(Data) ->
  Status = get_job_value("status.current", Data),
  Time = get_job_value("status." ++ maybe_helper:maybe_string(Status) ++ ".time", Data),
  
  {struct, [{<<"current">>, Status},
            {<<"time">>,    Time}]}.
  
expand_job_tasks(Data) ->
  Tasks = get_job_tasks(Data),
  expand_job_tasks(Tasks, Data, []).
  
expand_job_tasks([Task | Others], Data, Acc) ->
  Struct = {struct, [{<<"name">>,   maybe_helper:maybe_binary(Task)},
                     {<<"status">>, expand_task_status(Task, Data)},
                     {<<"data">>,   get_task_value(Task, "output.data", Data)}]},
  expand_job_tasks(Others, Data, [Struct | Acc]);

expand_job_tasks([], _Data, Acc) ->
  Acc.

expand_task_status(Task, Data) ->
  Status = get_task_value(Task, "status.current", Data),
  Time = get_task_value(Task, "status." ++ maybe_helper:maybe_string(Status) ++ ".time", Data),

  {struct, [{<<"current">>, Status},
            {<<"time">>,    Time}]}.

get_value(Key, Data) ->
  maybe_helper:maybe_binary(proplists:get_value(Key, Data)).

get_job_value(Key, Data) ->
  get_value("job." ++ Key, Data).

get_job_tasks(Data) ->
  re:split(proplists:get_value("job.tasks", Data), ",").

get_task_value(Task, Key, Data) ->
  get_value("task." ++ maybe_helper:maybe_string(Task) ++ "." ++ Key, Data).
