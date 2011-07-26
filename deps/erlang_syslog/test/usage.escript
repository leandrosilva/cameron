#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%%====================================================================
%% An usage test as simple and direct as possible. Enjoy.
%%====================================================================

main(_) ->
  io:format("~n-------------------------------------------~n"),
  io:format("Usage test :: Take a look at Syslog please."),
  io:format("~n-------------------------------------------~n"),
  
  test_simple(),
  test_advanced().

test_simple() ->
  syslog:start_link(),
  
  syslog:send("(simple) info message"),

  ok.
  
test_advanced() ->
  syslog:start_link(my_syslog, my_app, "localhost", 514, local0),
  
  syslog:send(my_syslog, "(advanced - send) debug message",     [{level, debug}]),
  syslog:send(my_syslog, "(advanced - send) info message",      [{level, info}]),
  syslog:send(my_syslog, "(advanced - send) notice message",    [{level, notice}]),
  syslog:send(my_syslog, "(advanced - send) warning message",   [{level, warning}]),
  syslog:send(my_syslog, "(advanced - send) error message",     [{level, error}]),
  syslog:send(my_syslog, "(advanced - send) critical message",  [{level, critical}]),
  syslog:send(my_syslog, "(advanced - send) alert message",     [{level, alert}]),
  syslog:send(my_syslog, "(advanced - send) emergency message", [{level, emergency}]),

  syslog:debug(my_syslog, "(advanced - debug) debug message"),
  syslog:info(my_syslog, "(advanced - info) info message"),
  syslog:notice(my_syslog, "(advanced - notice) notice message"),
  syslog:warning(my_syslog, "(advanced - warning) warning message"),
  syslog:error(my_syslog, "(advanced - error) error message"),
  syslog:critical(my_syslog, "(advanced - critical) critical message"),
  syslog:alert(my_syslog, "(advanced - alert) alert message"),
  syslog:emergency(my_syslog, "(advanced - emergency) emergency message"),
  
  ok.
