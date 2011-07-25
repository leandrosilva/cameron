%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-define(pname(UUID), list_to_atom("cameron_" ++ UUID)).

% activity and process definition
-record(activity_definition, {name, url}).
-record(process_definition,  {name, start_activity = #activity_definition{}}).

% job and activity instances in/out parameters
-record(job_input,       {key, data, requestor}).
-record(activity_input,  {key, data, requestor}).
-record(activity_output, {data, next_activities}).

% an instance of a process definition
-record(job, {uuid,
              process = #process_definition{},
              input   = #job_input{}}).

% an instance of an activity related to a job given
-record(activity, {definition   = #activity_definition{},
                   context_job  = #job{},
                   input        = #activity_input{},
                   output       = #activity_output{},
                   failed       = no}).
