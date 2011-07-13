%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-record(workflow, {name, start_url}).

-record(workflow_request, {workflow = #workflow{}, key, data, from}).

-record(workflow_ticket, {workflow_name, key, uuid}).

-record(workflow_step_input, {workflow_name, ticket_uuid, name, url, payload, worker_name}).

-record(workflow_step_output, {workflow_name, ticket_uuid, name, url, payload, output, worker_name}).
