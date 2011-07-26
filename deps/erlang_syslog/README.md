### Enable UDP

Ensure that syslogd has udp sockets enabled:
[OS X](http://stackoverflow.com/questions/1185554/how-to-enable-syslogd-to-receive-udp-logs-from-routers-in-osx)

### Build

    make

### Log

    0> syslog:start_link(name, tag, "localhost", 514, local0).
    ok
    2> syslog:send(name, "test").
    ok
    3> syslog:notice(name, "other test").
    ok

### Logged

    $ syslog
    ...
    Tue Mar 16 18:36:48 192.168.1.101  tag[4294967295] <Info>: test
    Tue Mar 16 18:36:57 192.168.1.101  tag[4294967295] <Notice>: other test
