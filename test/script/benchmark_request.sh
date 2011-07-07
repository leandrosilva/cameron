#!/bin/sh

ab -n 10000 -c 50 -p benchmark_request.json http://localhost:8080/api/diagnostic/ask
