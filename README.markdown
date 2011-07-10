## cameron

That is a *work in progress* for **Cameron Workflow System** which aim to be a generic workflow system (as its name suggests, I know) able to handle multiple concurrent requests asking to run pre-defined workflows, enqueue them and run them in parallel, as fast as possible.

To achive that objective, as you can see, it has been built as an Erlang/OTP application with a REST-like Web API, powered by Misulting, and a Redis-based backend database and queue system.

### How does it work?

1. It receives a request asking to run a workflow given:

    POST http://{host}:{port}/api/workflow/{name}/start HTTP/1.1
    Content-Type: application/json
    
    { "key":  "any kind of ID which is gonna be posted to the start_point_url for that workflow",
      "data": "any JSON-like data to be attached to that resquest" }

Let see what these input variables mean:

- **host:** Cameron web server host name or IP address
- **port:** Cameron web server port

These values are statically defineds inside *priv/config/{environment}.config* which is selected by environment (e.g. development, test, or production).

- **name:** the unique identification of a workflow given

That value also is defined inside *priv/config/{environment}.config* with a specific layout we are going to see next.

Finally, that *JSON payload*, as itself explains, has a pre-defined (and expected) layout.
  
1.1. It verifies whether that workflow exists or not, by look to a configuration file where is recorded any existent workflow. The layout of that configuration file is like that:

    {workflows, [{workflow, {name, bar},
                            {start_point_url, "http://foo.com/workflow/bar/start/{key}"}},
                 {workflow, {name, zar},
                            {start_point_url, "http://foo.com/workflow/zar/start/{key}"}}]}

1.2.exists.1. It generates a ticket, thru cameron_ticket module, which is a kind of UUID for that request

1.2.exists.2. Pushes that ticket to a Redis queue:

    lpush cameron:workflow:{name}:queue:incoming {ticket}

1.2.exists.3. Creates a hash, named by that ticket, to stores any data about that request in its whole life, with has the following layout:

    hmset cameron:workflow:{name}:ticket:{ticket}
          payload.key           {from request}
          payload.data          {from request}
          status.current        enqueued
          status.enqueued.time  {now}

1.2.exists.4. Responds HTTP status 201 with Location header set to /api/workflow/{name}/ticket/{ticket}

1.2.exists.5. Notifies Dispatcher process which is responsable to pass that request/ticket away

1.2.not.1. It responds with HTTP status 404, meaning that workflow doesn't exist
    
2. Dispatcher is a gen_server which provides an public API to cameron_web_api notify incoming requests to a workflow given:

    cameron_dispatcher:notify_incoming_request_to(WorkflowName)

2.1. It pop a ticket from the incoming queue, at Redis, for that workflow:

    rpop cameron:workflow:{name}:queue:incoming

2.2. Updates ticket's hash with new current status, as following:

    hmset cameron:workflow:{name}:ticket:{ticket}
          status.current          dispatched
          status.dispatched.time  {now}

2.3. Spawns a new worker to handle that workflow/ticket

    {ok, WorkerPid} = cameron_worker:spawn_new_to(WorkflowName, Ticket)

3. Workers are gen_servers which are created/spawned by the Dispatcher on demand, one per request/ticket

3.1. So, when a worker receives a request to move on thru a workflow given, it starts by POST the payload's key to workflow's start_point_url

Just to remember, this start_point_url resides inside a configuration file, as showed above:

    {workflow, {name, bar},
               {start_point_url, "http://foo.com/workflow/bar/start/{key}"}

And that start_point_url should respond something like this:

    HTTP/1.1 200 OK
    
    { "name":     "bar",
      "data":     "any JSON-like data to be attached to that resquest",
      "next":     [{ "name": "kar",
                     "url":  "http://kar.com/workflow/{key}"},
                   { "name": "xar",
                     "url":  "http://xar.com/workflow/{key}"}],
      "parallel": "yes" }

What else?????? What else??????
