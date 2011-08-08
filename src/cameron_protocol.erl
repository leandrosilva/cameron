%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc JSON-based protocol.

-module(cameron_protocol).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([parse_request_payload/1, build_response_payload/2]).

%% @spec parse_request_payload(Payload) -> {Key, Data, Requestor} | {error, Reason}
%% @doc Parses payload string from client request.
parse_request_payload(Payload) ->
  Struct = struct:from_json(Payload),
  
  Key = struct:get_value(<<"key">>, Struct, {format, list}),
  Data = struct:get_value(<<"data">>, Struct, {format, list}),
  Requestor = struct:get_value(<<"requestor">>, Struct, {format, list}),
  
  {Key, Data, Requestor}.

%% @spec build_response_payload({ProcessName, Key, UUID}, Data) -> Struct | {error, Reason}
%% @doc Parses payload string from client request.
build_response_payload({ProcessName, Key, UUID}, Data) ->
  Struct = {struct, [{<<"process">>,   eh_maybe:maybe_binary(ProcessName)},
                     {<<"uuid">>,      eh_maybe:maybe_binary(UUID)},
                     {<<"key">>,       eh_maybe:maybe_binary(Key)},
                     {<<"requestor">>, get_job_value("requestor", Data)},
                     {<<"status">>,    expand_job_status(Data)},
                     {<<"tasks">>,     expand_job_tasks(Data)}]},
  struct:to_json(Struct).

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
