%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-define(pname(UUID), list_to_atom("cameron_job_" ++ UUID)).

% activity definition
-record(activity_definition, {name, url}).

% process definition
-record(process_definition,  {name, start_activity = #activity_definition{}}).

% job is an instance of a process definition, when that's scheduled to be executed
-record(job_input, {key, data, requestor}).

-record(job, {uuid,
              process = #process_definition{},
              input   = #job_input{}}).

% an instance of an activity in the context of a job
-record(task_input,  {key, data, requestor}).
-record(task_output, {data, next_activities}).

-record(task, {context_job  = #job{},
               activity     = #activity_definition{},
               input        = #task_input{},
               output       = #task_output{},
               failed       = no}).
