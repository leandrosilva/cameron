## cameron

This is a **work in progress** for **Cameron Workflow Engine** whose aim is to be a web-based process engine able to handle with multiple concurrent HTTP requests asking for running pre-defined [but in the same time dynamic] processes, enqueue these requests as jobs, and run these jobs in parallel as soon as possible.

By web-based I mean:

* It exposes a Web API
* And works on process workflows thru their Web API

So if you have any background job that must cascade many tasks to achieve a goal, maybe Cameron fits to your needs.

To achieve this objective, as you can see, Cameron has been built as an [Erlang/OTP](http://www.erlang.org/doc) application with a REST-like Web API, powered by [Misultin](https://github.com/ostinelli/misultin), and a [Redis](http://redis.io)-based backend database.

If you would like to have a briefly introduction, please take a look at **doc/cameron\_pres.pdf**.

### Is it aiming the Real World?

Yes. I have been working in this piece of software because I have a real demand on my Real World job. It has been pretty fun do write it, but it has a serious target.

However, for the time being, we are not using it in production yet. **It still new and unended** _-- yes, needless to say this, actually --_, so we are testing and incubating it before. Nobody here want to be called in the late night hours, you know.

### Pieces of it

Among Cameron's modules there are a couple of those I think is important to mention here:

* **cameron\_process\_catalog** - Generic server whose manages the catalog of available process
* **cameron\_process\_sup** - Supervisor of generic servers on process domain
* **cameron\_job\_data** - Generic server whose interacts with Redis and manages process data
* **cameron\_job\_scheduler** - Generic server responsable to create, schedule and dispatch jobs to be done
* **cameron\_job\_runner** - Generic server whose get things done; it spawns new processes to handle each task of a process
* **cameron\_web\_server** - Generic server strongly based on Misultin
* **cameron\_web\_api** - Milsultin-based callback module to handle HTTP requests

### Getting Started

#### Installation

    git clone https://leandrosilva@github.com/leandrosilva/cameron.git

#### Configuration

Available workflows are registered in a file under **priv/processes**, based on environment and extension **.config**.

For example:

    priv/processes/development.config

With layout below:

    {processes, [{foo, {start_activity_url, "http://localhost:9292/foo/v0.0.1/start"}},
                 {bar, {start_activity_url, "http://bar.com/process/bar/start"}}]}.

#### Compiling

    make devrel    # for development environment
    make rel       # for production environment. It switches to use erlang_syslog instead of STDIO.

#### Running

    ./dev/cameron_dev/bin/cameron console    # for development
    ./dev/cameron_dev/bin/cameron start      # for development

    ./rel/cameron/bin/cameron start          # for production
    ./rel/cameron/bin/cameron console        # for production

    # Debian and Red Hat packets coming soon.  ;)

#### Testing

First terminal instance:

    redis-server

Second terminal instance:

    redis-client

Third terminal instance:

    cd $CAMERON/test/foo_workflow
    ./bin/run    # it requires Ruby 1.9.x and couple of gems

Forth terminal instance:

    cd $CAMERON
    make run_dev

Fifth terminal instance:

    cd $CAMERON
    ./test/script/request_for_404.sh

Back to second terminal instance, at redis-client prompt, and type:

    keys cameron:process:*

You should see nothing. So OK, now back to the fifth terminal instance, and type:

    ./test/script/request_for_foo.sh

Back to redis-client again and:

    keys cameron:process:*
    hgetall cameron:process:foo:key:(id,007):job:{UUID}    # uuid as you saw on last command

If everything is alright, you should see 86 entries at this hash.

#### Wait. What about automated tests?

I didn't write yet. Please wait a moment.

### How does it work?

#### Asking for a process

It waits for a HTTP POST asking to run a process workflow:

    curl -X POST http://localhost:8080/api/process/foo/start \
         -d '{"key":"(id,007)", "data":"be careful with that data", "requestor":"bob_the_thin"}' \
         -i \
         --header 'Content-Type: application/json'

And when it happens, if that required process workflow exists, Cameron will return something like it:

    HTTP/1.1 201 Created
    Connection: Keep-Alive
    Content-Length: 99
    Content-Type: application/json
    Location: http://localhost:8080/api/process/foo/key/(id,007)/job/63f44a1a36d8472a3c501b6fbc2e8825

    {"payload":"{"key":"(id,007)", "data":"be careful with that data", "requestor":"bob_the_thin"}"}

Otherwise, if that process does not exist, Cameron will return:

    HTTP/1.1 404 Not Found
    Connection: Keep-Alive
    Content-Length: 99
    Content-Type: application/json

    {"payload":"{"key":"(id,007)", "data":"be careful with that data", "requestor":"bob_the_thin"}"}

Since everything is alright -- I mean, the process workflow exists --, Cameron will create a job to do/run that process given, save its info in the Redis, and asynchronously dispatch it to run.

So this first step will result in a entry like below:

    hmset cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9
          job.key                   "(id,007)"
          job.data                  "be careful with that data"
          job.requestor             "bob_the_thin"
          job.status.current        "scheduled"
          job.status.scheduled.time "08-01-2011 18:03:00"

    set cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9:pending

#### Running a job

As soon as a job starts to run (or to be done, if you prefer), it is marked as running like below:

    hmset cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9
          status.current      "running"
          status.running.time "08-01-2011 18:03:00"

    set cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9:running

And a new **cameron\_job\_runner** is spawned to handle it.

#### Getting things done

The first thing **cameron\_job\_runner** does to get things do is to spawn an Erlang process to handle the **start activity** of the process workflow given.

An **activity** is the definition of a **task**, thus a task is a instance of an activity, the same way a job is an instance of a process. And following this thought, a **start activity** is the start point to a process workflow, as you already realized.

When it happens, it is registered in Redis as follow:

    hmset cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9
          task.start.status.current      "running"
          task.start.status.running.time "08-01-2011 18:03:00"

    set cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9:{task}:running

As you can see at **test/foo\_workflow\_/api/workflow.rb**, a process workflow is web-based, talks JSON both ways, and always it receives the original **request data** as its payload, like below:

    curl -X POST http://localhost:9292/foo/v0.0.1/start \
         -d '{"key":"(id,007)", "data":"be careful with that data", "requestor":"bob_the_thin"}' \
         -i \
         --header 'Content-Type: application/json'

An activity/task among other things, can return data and next_activities, as follow:

    {
        "process": "foo",
        "name": "whois",
        "requestor": "bob_the_thin",

        "data": {
            "who_id": "(id,007)",
            "who_name": "Leandro Silva",
            "who_login": "leandrosilva.codezone",
            "who_web_info": {
                "blog": "http://leandrosilva.com.br",
                "twitter": "codezone"
            },
            "who_dev_info": {
                "github": "http://github.com/leandrosilva"
            }
        },

        "next_activities": {
            "definitions": [{
                "name": "act_1",
                "url": "http://localhost:9292/foo/v0.0.1/activity/act_1"
            },
            {
                "name": "act_2",
                "url": "http://localhost:9292/foo/v0.0.1/activity/act_2"
            },
            {
                "name": "act_3",
                "url": "http://localhost:9292/foo/v0.0.1/activity/act_3"
            },
            {
                "name": "act_4",
                "url": "http://localhost:9292/foo/v0.0.1/activity/act_4"
            },
            {
                "name": "act_5",
                "url": "http://localhost:9292/foo/v0.0.1/activity/act_5"
            }]
        }
    }

What happens now? Basically it saves that response at Redis:

    hmset cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9
          task.start.status.current         "done"
          task.start.status.done.time       "08-01-2011 18:03:00"
          task.start.output.data            "..."
          task.start.output.next_activities "..."

    set cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9:{task}:done

And follow the same process recursively, the **data** attribute is passed to each **next_activities**.

Yep. It's pipeline-based. So you can cascade it to virtually infinity!

#### What about errors?

Yes, it happens -- *we are talking about real world software, isn't we?*

OK, when an error happens, Cameron saves its data and still working to get things done; the most, the better.

    hmset cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9
          task.start.status.current    "error"
          task.start.status.error.time "08-01-2011 18:03:00"
          task.start.output.data       "the awesome error message"

And it also creates an entry to say that an error happened:

    set cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9:{task}:error

Cool?

#### The end

When everything is done, it ends with a new information at Redis:

    hmset cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9
          job.status.current   "done"
          job.status.done.time "08-01-2011 18:03:00"

    set cameron:process:foo:key:(id,007):job:3bd73a7731e5efca25b5ef05e3f79af9:done

That is it.

#### Getting results

Just do HTTP GET at the location head contained in the HTTP response of the start of this whole process:

    http://localhost:8080/api/process/foo/key/(id,007)/job/63f44a1a36d8472a3c501b6fbc2e8825

It will result a JSON response with every data retrieved, including errors, just like that:

    HTTP/1.1 200 OK
    Connection: Keep-Alive
    Content-Length: 1634
    Content-Type: application/json

    {
        "process": "foo",
        "uuid": "a76970078ae93e874c9b1e2b2072c365",
        "key": "(id,007)",
        "requestor": "bob_the_thin",

        "status": {
            "current": "done",
            "time": "08-05-2011 19:45:26"
        },

        "tasks": [{
            "name": "start",
            "requestor": "bob_the_thin",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "who_id": "(id,007)",
                "who_name": "Leandro Silva",
                "who_login": "leandrosilva.codezone",
                "who_web_info": {
                    "blog": "http://leandrosilva.com.br",
                    "twitter": "codezone"
                },
                "who_dev_info": {
                    "github": "http://github.com/leandrosilva"
                }
            }
        },
        {
            "name": "act_1",
            "requestor": "whois",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "bar": "the likable bar",
                "baz": {
                    "qux": "the awesome qux",
                    "quux": "the amazing quux",
                    "corge": "the great corge"
                }
            }
        },
        {
            "name": "act_2",
            "requestor": "whois",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "bar": "the likable bar",
                "baz": {
                    "qux": "the awesome qux",
                    "quux": "the amazing quux",
                    "corge": "the great corge"
                }
            }
        },
        {
            "name": "act_3",
            "requestor": "whois",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "bar": "the likable bar",
                "baz": {
                    "qux": "the awesome qux",
                    "quux": "the amazing quux",
                    "corge": "the great corge"
                }
            }
        },
        {
            "name": "act_4",
            "requestor": "whois",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "bar": "the likable bar",
                "baz": {
                    "qux": "the awesome qux",
                    "quux": "the amazing quux",
                    "corge": "the great corge"
                }
            }
        },
        {
            "name": "act_5",
            "requestor": "whois",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "bar": "the likable bar",
                "baz": {
                    "qux": "the awesome qux",
                    "quux": "the amazing quux",
                    "corge": "the great corge"
                }
            }
        },
        {
            "name": "act_5_sub_1",
            "requestor": "foo_act_4",
            "status": {
                "current": "done",
                "time": "08-05-2011 19:45:26"
            },
            "data": {
                "grault": "the likable grault",
                "garply": {
                    "waldo": "the awesome waldo",
                    "fred": "the amazing fred",
                    "plugh": "the great plugh"
                }
            }
        }]
    }

Now, yes, that is it!

### What else?

There are many things to do. **It still new and unended**, as I said before. So stay tuned!

### And finally...

If you have time, please test it and give me feedback; and, if you have even more time, pleeease hack it and send me a pull request.

### Copyright

Copyright (c) 2011 Leandro Silva <leandrodoze@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
