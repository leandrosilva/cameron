%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-record(workflow_request, {workflow_name, key, data, from}).

-record(workflow_ticket, {workflow_name, uuid, short_uuid, tag_part, key_part, timestamp_part}).

-record(workflow_step_input, {workflow_name, ticket_short_uuid, name, url, payload, worker_name}).

-record(workflow_step_output, {workflow_name, ticket_short_uuid, name, url, payload, output, worker_name}).
