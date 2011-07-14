%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-record(workflow, {name,
                   start_url}).

-record(request, {workflow = #workflow{},
                  key,
                  data,
                  from}).

-record(promise, {uuid,
                  request = #request{}}).

-record(step_input, {promise = #promise{},
                     name,
                     url,
                     payload,
                     worker_name}).

-record(step_output, {step_input = #step_input{},
                      output}).
