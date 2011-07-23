#!/bin/sh

ab -n 10000 -c 50 -p test/script/benchmark_request.json http://localhost:8080/api/process/diagnostic/start
