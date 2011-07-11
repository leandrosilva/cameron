## cameron

That is a *work in progress* for **Cameron Workflow System** which aim to be a web-based workflow system able to handle multiple concurrent HTTP requests asking to run pre-defined workflows, enqueue them and run them in parallel, as fast as possible.

To achive that objective, as you can see, it has been built as an Erlang/OTP application with a REST-like Web API, powered by Misultin, and a Redis-based backend database and queue system.

### How does it work?

1.) It receives a request asking to run a workflow given:

    POST http://{host}:{port}/api/workflow/{name}/start HTTP/1.1
    Content-Type: application/json
    
    { "key":  "any kind of ID which is gonna be posted to the start_point_url for that workflow",
      "data": "any JSON-like data to be attached to that resquest",
      "from": "an identification of the client/requester" }

That incoming request is handled by **cameron_web_api** module, which is a HTTP handler module for **cameron_web_server**, a Misultin-based web server. So, let's see what these input variables mean:

- **host:** Cameron web server host name or IP address
- **port:** Cameron web server port

These values are statically defined inside **priv/config/{environment}.config** which is, at Cameron bootstrap, selected by environment (e.g. development, test, or production).

- **name:** the unique identification of a workflow given

That value is defined inside **priv/config/workflows.{environment}.config** with a specific layout we are going to see next.

And finally, that *JSON payload*, as itself explains, has an expected layout containing two attributes which are **key** and **data**.

Oops! So, to be idiomatic, there is a record for workflow request:

    -record(workflow_request, {workflow_name, key, data, from}).

1.1.) It verifies whether that workflow exists or not, by look to a configuration file where is recorded any existent workflow. The layout of that configuration file is like that:

    {workflows, [{workflow, {name, bar},
                            {start_point_url, "http://foo.com/workflow/bar/start/{key}"}},
                 {workflow, {name, zar},
                            {start_point_url, "http://foo.com/workflow/zar/start/{key}"}}]}

1.2.exists.1.) It generates a ticket, thru **cameron_ticket** module, which is a kind of UUID for that request

A **Ticket UUID** is like this:

    cameron:workflow:{name}:ticket:{key}:{timestamp}

And has its own record:

    -record(workflow_ticket, {workflow_name, uuid, short_uuid, tag_part, key_part, timestamp_part}).

Where:

- **workflow_name:** workflow name, as we could see before
- **uuid:** full length Ticket UUID
- **short_uuid:** short length Ticket UUD. It will be explained in more details next
- **tag_part:** cameron:workflow:{name}:ticket:
- **key_part:** request payload's key attribute
- **timestamp_part:** yyyyMMddhhmmssms, that is year + hour + minute + second + microseconds (16 digits)

#### Important information about Ticket here

Clients/requesters know tickets just like it:

    {key}:{timestamp}

In other words, to a client:

    Ticket = {key}:{timestamp}

And not:

    Ticket = cameron:workflow:{name}:ticket:{key}:{timestamp}

Thus, internally, we call theses different point of views:

- **long ticket uuid**

*"Full Length UUID"*. It is how a ticket UUID is stored in Redis:

    cameron:workflow:{name}:ticket:{key}:{timestamp}
    
- **short ticket uuid**

*"Short Length UUID"*. That is how a ticket UUID is know by clients:

    {key}:{timestamp}

These different point of views is just to be brief. I mean, clients don't need to deal with a loooong ticket UUID, with a Redis-like namespace.

#### Other important information about Ticket here

Here we have a kind of tip. If it is convenient for you, key can be a tuple. Let's see does it work:

    cameron:workflow:{name}:ticket:{(key_type,key_value)}:{timestamp}

In order words, it is:

    cameron:workflow:bar:ticket:(login,leandrosilva):20110710213523
    cameron:workflow:bar:ticket:(id,007):20110710213645
    cameron:workflow:bar:ticket:(cpf,28965487611):20110710213715

Yay. That is fun, isn't that?

1.2.exists.2.) Pushes that ticket to a Redis queue:

    lpush cameron:workflow:{name}:queue:incoming {ticket}

1.2.exists.3.) Creates a Redis hash, named by that ticket, to store any data about that request in its whole life, with has the following layout:

    hmset cameron:workflow:{name}:ticket:{key}:{timestamp}
          request.key           {from request payload}
          request.data          {from request payload}
          request.from          {from request payload}
          status.current        enqueued
          status.enqueued.time  {now}

1.2.exists.4.) Notifies **cameron_dispatcher** process which is responsible to pass that request/ticket away:

    cameron_dispatcher:notify_incoming_request(WorkflowName)

1.2.exists.5.) Finally it responds to the HTTP resquest with sucess:

    HTTP/1.1 201 Created
    Location: http://{host}:{port}/api/workflow/{name}/ticket/{ticket}
    Content-Type: application/json

1.2.not.1.) Otherwise, it responds the HTTP resquest with an error:

    HTTP/1.1 404 Nof Found
    Content-Type: application/json

    {that received payload, to requester/client know what happened}
    
2.) **cameron\_dispatcher** is a *gen_server* which provides an public API to be notified by **cameron\_web_api** about incoming requests, as we already saw before

2.1.) It take a ticket from the incoming queue, at Redis, for that workflow:

    rpop cameron:workflow:{name}:queue:incoming

2.2.) Updates ticket's hash with new current status, as following:

    hset cameron:workflow:{name}:ticket:{key}:{timestamp}
         status.current          dispatched
         status.dispatched.time  {now}

2.3.) Spawns a new worker to handle that workflow/ticket

    {ok, WorkerPid} = cameron_worker:spawn_new(WorkflowName, Ticket)

3.) **cameron\_worker** is a *gen_server* which is created/spawned by the **cameron_dispatcher** on demand. In other words, one new process per request/ticket

Its state record is like that:

    -record(state, {name, ticket, countdown, workflow_request}).

And there is also a **cameron_worker** supervisor, that is **cameron_worker_sup**.

3.1.) So, when a worker receives a request to move on thru a workflow given, it starts by POST the payload's key to workflow's start_point_url

Just to remember, this start_point_url resides inside **priv/config/workflows.{environment}.config**, as we already saw before:

    {workflow, {name, bar},
               {start_point_url, "http://foo.com/workflow/bar/start/{key}"}

And so, that **start_point_url**, when receives a HTTP POST, must respond something like this:

    HTTP/1.1 200 OK
    
    { "workflow_name":      "bar",
      "step_name":          "start_point"
      "step_type":          "parallel or pipeline",
      "step_data":          "any JSON-like data to be attached to that resquest",
      "next_steps":         [{ "name":    "kar",
                               "url":     "http://kar.com/workflow/{key}",
                               "payload": "any JSON-like data to be posted to this step"},
                             { "name":    "xar",
                               "url":     "http://xar.com/workflow/{key}",
                               "payload": "any JSON-like data to be posted to this step"}] }

This response really means:

- **workflow_name:** self explained
- **name:** workflow name
- **type:** "parallel" means each step must run in parallel, independently; if "pipeline" each step will run sequentially and its response will be passed to next one
- **data:** any kind of JSON-like data
- **steps:** list of steps that make up the workflow

3.2.) Updates ticket's hash with new current status, as following:

    hset cameron:workflow:{name}:ticket:{key}:{timestamp}
         status.current       started
         status.started.time  {now}
         step.kar.name        {kar.name}
         step.kar.url         {kar.url}
         step.kar.payload     {kar.payload}
         step.xar.name        {xar.name}
         step.xar.url         {xar.url}
         step.xar.payload     {xar.payload}

3.3.) For each step **cameron_worker** spawn a new process passing a record as parameter:

    -record(workflow_step_input, {workflow_name, ticket_short_uuid, name, url, payload, worker_name}).

And updates ticket's hash:

    hset cameron:workflow:{name}:ticket:{key}:{timestamp}
         step.{name}.status               spawned
         step.{name}.status.spawned.time  {now}

3.4.) After spawn every **slaver** process, updates ticket's hash with new current status, as following:

    hset cameron:workflow:{name}:ticket:{key}:{timestamp}
         status.current   wip
         status.wip.time  {now}

3.5.) Once a **slaver** process finish its work, it notifies its **cameron_worker** owner and past to that a record with result of its work:

    -record(workflow_step_output, {workflow_name, ticket_short_uuid, name, url, payload, output, worker_name}).

And updates ticket's hash:

    hset cameron:workflow:{name}:ticket:{key}:{timestamp}
         step.{name}.status            done
         step.{name}.status.done.time  {now}
         step.{name}.output            {output}

3.6.) **cameron_worker** has a kind of countdown in its process state, as we saw above, which is used to know when its whole work is done. And when its done:

It updates ticket's hash:

    hset cameron:workflow:{name}:ticket:{key}:{timestamp}
         status.current    done
         status.done.time  {now}

And just stop.

Yay! **When this whole process is done**, the workflow is done.

### How does a workflow state is get by the client?

It is possible to see any available data (inside ticket's hash) at any time by:

    GET http://{host}:{port}/api/workflow/{name}/ticket/{ticket} HTTP/1.1
    Accept: application/json

And that is the "search semantic" on Redis for achieve it:

    hget cameron:workflow:{name}:ticket:{ticket}

Or, you can search by key:

    GET http://{host}:{port}/api/workflow/{name}/key/{key} HTTP/1.1
    Accept: application/json

Which can be achieved by:

    key cameron:workflow:{name}:ticket:{key}:*

### What else?

- TODO
