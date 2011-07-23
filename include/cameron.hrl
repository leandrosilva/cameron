%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-define(pname(UUID), list_to_atom("cameron_" ++ UUID)).

-record(process,     {name,
                      start_url}).
                    
-record(request,     {process = #process{},
                      key,
                      data,
                      from}).
                    
-record(job,         {uuid,
                      request = #request{}}).

-record(task_input,  {job = #job{},
                      name,
                      pname}).

-record(task_output, {task_input = #task_input{},
                      output}).
