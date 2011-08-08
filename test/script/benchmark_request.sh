#!/bin/sh

# nooo! ~ 150 rps
# ab -n 10000 -c 50 -p test/script/benchmark_request.json http://localhost:8080/api/process/foo/start

# not so good ~ 55 rps
# ab -n 10000 -c 5 -p test/script/benchmark_request.json http://localhost:8080/api/process/foo/start

# ok ~ 500 rps
ab -n 1000 -c 30 -p test/script/benchmark_request.json http://localhost:8080/api/process/foo/start
