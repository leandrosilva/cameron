%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-define(Pname(UUID), list_to_atom("cameron_" ++ UUID)).

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
                     pname}).

-record(step_output, {step_input = #step_input{},
                      output}).
