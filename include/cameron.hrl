%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%%
%% Global define ----------------------------------------------------------------------------------
%%

-define(pname(UUID), list_to_atom("cameron_" ++ UUID)).

%%
%% Data model -------------------------------------------------------------------------------------
%%

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
-record(task_output, {data, next_activities = undefined}).

-record(task, {context_job    = #job{},
               activity       = #activity_definition{},
               input          = #task_input{},
               output         = #task_output{},
               failed         = no}).

%%
%% Log levels -------------------------------------------------------------------------------------
%%

-ifdef(use_syslog).

-define(DEBUG(Msg),            erlang:apply(syslog, debug, [cameron_syslog, Msg])).
-define(DEBUG(Msg, Args),      erlang:apply(syslog, debug, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(INFO(Msg),             erlang:apply(syslog, info, [cameron_syslog, Msg])).
-define(INFO(Msg, Args),       erlang:apply(syslog, info, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(NOTICE(Msg),           erlang:apply(syslog, notice, [cameron_syslog, Msg])).
-define(NOTICE(Msg, Args),     erlang:apply(syslog, notice, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(WARNING(Msg),          erlang:apply(syslog, warning, [cameron_syslog, Msg])).
-define(WARNING(Msg, Args),    erlang:apply(syslog, warning, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(ERROR(Msg),            erlang:apply(syslog, error, [cameron_syslog, Msg])).
-define(ERROR(Msg, Args),      erlang:apply(syslog, error, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(CRITICAL(Msg),         erlang:apply(syslog, critical, [cameron_syslog, Msg])).
-define(CRITICAL(Msg, Args),   erlang:apply(syslog, critical, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(ALERT(Msg),            erlang:apply(syslog, alert, [cameron_syslog, Msg])).
-define(ALERT(Msg, Args),      erlang:apply(syslog, alert, [cameron_syslog, io_lib:format(Msg, Args)])).

-define(EMERGENCY(Msg),        erlang:apply(syslog, emergency, [cameron_syslog, Msg])).
-define(EMERGENCY(Msg, Args),  erlang:apply(syslog, emergency, [cameron_syslog, io_lib:format(Msg, Args)])).

-else.

-define(DEBUG(Msg),            erlang:apply(io, format, ["[DEBUG] " ++ Msg ++ "~n"])).
-define(DEBUG(Msg, Args),      erlang:apply(io, format, ["[DEBUG] " ++ Msg ++ "~n", Args])).

-define(INFO(Msg),             erlang:apply(io, format, ["[INFO] " ++ Msg ++ "~n"])).
-define(INFO(Msg, Args),       erlang:apply(io, format, ["[INFO] " ++ Msg ++ "~n", Args])).

-define(NOTICE(Msg),           erlang:apply(io, format, ["[NOTICE] " ++ Msg ++ "~n"])).
-define(NOTICE(Msg, Args),     erlang:apply(io, format, ["[NOTICE] " ++ Msg ++ "~n", Args])).

-define(WARNING(Msg),          erlang:apply(io, format, ["[WARNING] " ++ Msg ++ "~n"])).
-define(WARNING(Msg, Args),    erlang:apply(io, format, ["[WARNING] " ++ Msg ++ "~n", Args])).

-define(ERROR(Msg),            erlang:apply(io, format, ["[ERROR] " ++ Msg ++ "~n"])).
-define(ERROR(Msg, Args),      erlang:apply(io, format, ["[ERROR] " ++ Msg ++ "~n", Args])).

-define(CRITICAL(Msg),         erlang:apply(io, format, ["[CRITICAL] " ++ Msg ++ "~n"])).
-define(CRITICAL(Msg, Args),   erlang:apply(io, format, ["[CRITICAL] " ++ Msg ++ "~n", Args])).

-define(ALERT(Msg),            erlang:apply(io, format, ["[ALERT] " ++ Msg ++ "~n"])).
-define(ALERT(Msg, Args),      erlang:apply(io, format, ["[ALERT] " ++ Msg ++ "~n", Args])).

-define(EMERGENCY(Msg),        erlang:apply(io, format, ["[EMERGENCY] " ++ Msg ++ "~n"])).
-define(EMERGENCY(Msg, Args),  erlang:apply(io, format, ["[EMERGENCY] " ++ Msg ++ "~n", Args])).

-endif.
