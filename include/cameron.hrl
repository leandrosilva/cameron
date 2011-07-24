%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-define(pname(UUID), list_to_atom("cameron_" ++ UUID)).

% a process definition
-record(process, {name, start_activity_url}).

% an instance of a process
-record(job, {process = #process{}, uuid, key, input, requestor}).

% an activity related to a job
-record(activity, {job = #job{}, name, url, key, input, requestor, output, failed = no}).
