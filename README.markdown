## cameron

That's a *work in progress* for **Cameron Workflow System** which aim to be a web-based workflow system able to handle multiple concurrent HTTP requests asking to run pre-defined workflows, enqueue them and run them in parallel, as fast as possible.

To achive that objective, as you can see, it has been built as an Erlang/OTP application with a REST-like Web API, powered by Misultin, and a Redis-based backend database and queue system.

### How does it work?

1.) It receives a request asking to run a workflow given:

    POST http://{host}:{port}/api/workflow/{name}/start HTTP/1.1
    Content-Type: application/json
    
    { "key":  "any kind of ID which is gonna be posted to the start_url for that workflow",
      "data": "any JSON-like data to be attached to that resquest",
      "from": "an identification of the client/requester" }

That incoming request is handled by **cameron_web_api** module, which is a HTTP handler module for **cameron_web_server**, a Misultin-based web server. So, let's see what these input variables mean:

- **host:** Cameron web server host name or IP address
- **port:** Cameron web server port

These values are statically defined inside **priv/config/{environment}.config** which is, at Cameron bootstrap, selected by environment (e.g. development, test, or production).

- **name:** the unique identification of a workflow given

That value is defined inside **priv/workflows/{environment}.config** with a specific layout we are going to see next.

And finally, that *JSON payload*, as itself explains, has an expected layout containing two attributes which are **key** and **data**.

Oops! So, to be idiomatic, there is a record for workflow request:

    -record(request, {workflow_name, key, data, from}).

1.1.) It verifies whether that workflow exists or not, by look to a configuration file where is recorded any existent workflow. The layout of that configuration file is like that:

    {workflows, [{bar, {start_url, "http://foo.com/workflow/bar/start/{key}"}},
                 {zar, {start_url, "http://foo.com/workflow/zar/start/{key}"}}]}.

1.2.exists.1.) It generates a request, thru **cameron_workflow_keeper** module, which is a kind of UUID for that request

In Redis, a **Request UUID** is stored like this:

    cameron:workflow:{name}:key:{key}:promise:{uuid}

That's what I call *full length UUID*, and isn't what customers know. Instead, customers know just the *UUID* part of that.

There's a record type to handle request which is:

    -record(request, {workflow_name, key, uuid}).

Where:

- **workflow_name:** workflow name, as we could see before
- **key:** request payload's key attribute
- **uuid:** a 32 bytes unique alphanumeric identifier

#### Important information about Request here

Here we have a kind of tip. If it is convenient for you, key can be a tuple. Let's see does it work:

    cameron:workflow:{name}:key:{(key_type,key_value)}:promise:{uuid}

In order words, it is:

    cameron:workflow:bar:key:(login,leandrosilva):promise:d858a86eb936a0bb83276299a1840086
    cameron:workflow:bar:key:(id,007):promise:3c43602bff3a7b1870577e828626aa7a
    cameron:workflow:bar:key:(cpf,28965487611):promise:a0b9c506344794e237ab8dd4ce24dcc3

Yay. That's fun, isn't that?

1.2.exists.2.) Pushes that request to a Redis queue:

    lpush cameron:workflow:{name}:queue:promise:pending {promise}

1.2.exists.3.) Creates a Redis hash, named by that request, to store any data about that request in its whole life, with has the following layout:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          request.key           {from request payload}
          request.data          {from request payload}
          request.from          {from request payload}
          status.current        promised
          status.promised.time  {now}

1.2.exists.4.) Notifies **cameron_workflow_dispatcher** process which is responsible to pass that request/request away:

    cameron_workflow_dispatcher:dispatch_new_promise(WorkflowName)

1.2.exists.5.) Finally it responds to the HTTP resquest with sucess:

    HTTP/1.1 201 Created
    Location: http://{host}:{port}/api/workflow/{name}/promise/{promise}
    Content-Type: application/json

1.2.not.1.) Otherwise, it responds the HTTP resquest with an error:

    HTTP/1.1 404 Nof Found
    Content-Type: application/json

    {that received payload, to requester/client know what happened}
    
2.) **cameron\_dispatcher** is a *gen_server* which provides an public API to be notified by **cameron\_web_api** about incoming requests, as we already saw before

2.1.) It take a request from the incoming queue, at Redis, for that workflow:

    rpop cameron:workflow:{name}:queue:promise:pending

2.2.) Updates request's hash with new current status, as following:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          status.current          dispatched
          status.dispatched.time  {now}

2.3.) Spawns a new worker to handle that workflow/request

    {ok, WorkerPid} = cameron_workflow_handler:pay_it(WorkflowName, Request)

3.) **cameron\_worker** is a *gen_server* which is created/spawned by the **cameron_workflow_dispatcher** on demand. In other words, one new process per request/request

Its state record is like that:

    -record(state, {name, request, countdown, request}).

And there is also a **cameron_workflow** supervisor, that's **cameron_workflow_sup**.

3.1.) So, when a worker receives a request to move on thru a workflow given, it starts by POST the payload's key to workflow's start_url

Just to remember, this start_url resides inside **priv/workflows/{environment}.config**, as we already saw before:

    {workflows, [{bar, {start_url, "http://foo.com/workflow/bar/start/{key}"}},
                 {zar, {start_url, "http://foo.com/workflow/zar/start/{key}"}}]}.

And so, that **start_url**, when receives a HTTP POST, must respond something like this:

    HTTP/1.1 200 OK
    
    { "workflow_name":      "bar",
      "step_name":          "start_point"
      "step_type":          "parallel or sequential",
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
- **type:** "parallel" means each step must run in parallel, independently; if "sequential" each step will run sequentially and its response will be passed to next one
- **data:** any kind of JSON-like data
- **steps:** list of steps that make up the workflow

3.2.) Updates request's hash with new current status, as following:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          status.current       started
          status.started.time  {now}
          step.kar.name        {kar.name}
          step.kar.url         {kar.url}
          step.kar.payload     {kar.payload}
          step.xar.name        {xar.name}
          step.xar.url         {xar.url}
          step.xar.payload     {xar.payload}

3.3.) For each step **cameron_workflow** spawn a new process passing a record as parameter:

    -record(step_input, {workflow_name, request_short_uuid, name, url, payload, worker_name}).

And updates request's hash:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          step.{name}.status.current       spawned
          step.{name}.status.spawned.time  {now}

3.4.) After spawn every **slaver** process, updates request's hash with new current status, as following:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          status.current   wip
          status.wip.time  {now}

3.5.) Once a **slaver** process finish its work, it notifies its **cameron_workflow** owner and past to that a record with result of its work:

    -record(step_output, {workflow_name, request_short_uuid, name, url, payload, output, worker_name}).

And updates request's hash:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          step.{name}.status.current    done
          step.{name}.status.done.time  {now}
          step.{name}.output            {output}

3.6.) **cameron_workflow** has a kind of countdown in its process state, as we saw above, which is used to know when its whole work is done. And when its done:

It updates request's hash:

    hmset cameron:workflow:{name}:key:{key}:promise:{uuid}
          status.current    done
          status.done.time  {now}

And just stop.

Yay! **When this whole process is done**, the workflow is done.

### How does a workflow state is get by the client?

It is possible to see any available data (inside request's hash) at any time by:

    GET http://{host}:{port}/api/workflow/{name}/key/{key}/promise/{promise} HTTP/1.1
    Accept: application/json

And that's the "search semantic" on Redis for achieve it:

    hget cameron:workflow:{name}:key:{key}:promise:{promise}

Or, you can search by key:

    GET http://{host}:{port}/api/workflow/{name}/key/{key} HTTP/1.1
    Accept: application/json

Which can be achieved by:

    key cameron:workflow:{name}:key:{key}:promise:*

### What else?

- need more documentation (TODO)

### What to do now?

- reengineer dispatcher, workers, and so on (embody "workflow" nomenclature)
- improve API
- finalize implementation (at least what is specified)
- elasticsearch integration to improve searching?
