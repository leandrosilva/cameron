%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

{application, cameron,
 [{description, "Cameron Diagnostic System"},
  {vsn, "0.0.1"},
  {modules, [
    cameron,
    cameron_app,
    cameron_sup,
    cameron_dispatcher,
    cameron_worker,
    cameron_messaging,
    cameron_web_server,
    cameron_web_api,
    cameron_deps
  ]},
  {registered, []},
  {mod, {cameron_app, []}},
  {env, [{web_server, [{host, "0.0.0.0"},
                       {port, 8080},
                       {backlog, 128},
                       {docroot, "priv/www"}]},
         {amqp_server, [{host, "127.0.0.1"}]}]},
  {applications, [kernel, stdlib]}]}.
