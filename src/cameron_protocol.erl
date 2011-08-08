%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc JSON-based protocol.

-module(cameron_protocol).
-author('Leandro Silva <leandrodoze@gmail.com>').

% api for web api
-export([parse_request_payload/1, build_response_payload/2]).
% api for task execution
-export([build_request_payload/2, parse_response_payload/1]).

%%
%% Includes and Records ---------------------------------------------------------------------------
%%

-include("cameron.hrl").

%% @spec parse_request_payload(Payload) -> {Key, Data, Requestor} | {error, Reason}
%% @doc Parses a JSON string from client request.
parse_request_payload(Payload) ->
  Struct = struct:from_json(Payload),
  
  Key = struct:get_value(<<"key">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, list}),
  Requestor = struct:get_value(<<"requestor">>, Struct, {format, list}),
  
  {Key, Data, Requestor}.

%% @spec build_response_payload({ProcessName, Key, UUID}, Data) -> Struct | {error, Reason}
%% @doc Builds JSON string to respond a client request.
build_response_payload({ProcessName, Key, UUID}, Data) ->
  Struct = {struct, [{<<"process">>,   eh_maybe:maybe_binary(ProcessName)},
                     {<<"uuid">>,      eh_maybe:maybe_binary(UUID)},
                     {<<"key">>,       eh_maybe:maybe_binary(Key)},
                     {<<"requestor">>, get_job_value("requestor", Data)},
                     {<<"status">>,    expand_job_status(Data)},
                     {<<"tasks">>,     expand_job_tasks(Data)}]},
  struct:to_json(Struct).

%% @spec build_request_payload(Job, {Data, Requestor}) -> JSONString | {error, Reason}
%% @doc Builds a JSON string to request a task execution.
build_request_payload(Job, {Data, Requestor}) ->
  #job{uuid  = UUID,
       input = #job_input{key = Key}} = Job,

  RequestPayload = struct:to_json({struct, [{<<"job">>, list_to_binary(UUID)},
                                            {<<"key">>, list_to_binary(Key)},
                                            {<<"data">>, list_to_binary(Data)},
                                            {<<"requestor">>, list_to_binary(Requestor)}]}),

  unicode:characters_to_list(RequestPayload).

%% @spec build_request_payload(Job, {Data, Requestor}) -> JSONString | {error, Reason}
%% @doc Parses a JSON string of a task result.
parse_response_payload(ResponsePayload) ->
  Struct = struct:from_json(ResponsePayload),

  Name = struct:get_value(<<"name">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, json}),
  NextActivities = struct:get_value(<<"next_activities">>, Struct, {format, json}),

  {Name, Data, NextActivities}.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

% --- helpers to build_response_payload function --------------------------------------------------

expand_job_status(Data) ->
  Status = get_job_value("status.current", Data),
  Time = get_job_value("status." ++ eh_maybe:maybe_string(Status) ++ ".time", Data),

  {struct, [{<<"current">>, Status},
            {<<"time">>,    Time}]}.

expand_job_tasks(Data) ->
  Tasks = get_job_tasks(Data),
  expand_job_tasks(Tasks, Data, []).

expand_job_tasks([Task | Others], Data, Acc) ->
  Struct = {struct, [{<<"name">>,      eh_maybe:maybe_binary(Task)},
                     {<<"requestor">>, get_task_value(Task, "requestor", Data)},
                     {<<"status">>,    expand_task_status(Task, Data)},
                     {<<"data">>,      expand_task_output(Task, Data)}]},
  expand_job_tasks(Others, Data, [Struct | Acc]);

expand_job_tasks([], _Data, Acc) ->
  lists:reverse(Acc).

expand_task_status(Task, Data) ->
  Status = get_task_value(Task, "status.current", Data),
  Time = get_task_value(Task, "status." ++ eh_maybe:maybe_string(Status) ++ ".time", Data),

  {struct, [{<<"current">>, Status},
            {<<"time">>,    Time}]}.

expand_task_output(Task, Data) ->
  case get_task_value(Task, "output.data", Data) of
    undefined ->
      <<"nothing yet">>;
    Binary ->
      String = binary_to_list(Binary),
      try struct:from_json(String) catch _:_ -> Binary end
  end.

get_value(Key, Data) ->
  eh_maybe:maybe_binary(proplists:get_value(Key, Data)).

get_job_value(Key, Data) ->
  get_value("job." ++ Key, Data).

get_job_tasks(Data) ->
  case proplists:get_value("job.tasks", Data) of
    undefined -> [];
    Tasks     -> re:split(Tasks, ",")
  end.

get_task_value(Task, Key, Data) ->
  get_value("task." ++ eh_maybe:maybe_string(Task) ++ "." ++ Key, Data).
