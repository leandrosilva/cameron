#!/bin/sh

# for benchmark:
#     ab -n 10000 -c 50 -p benchmark_request.json http://localhost:8080/api/diagnostic/ask

curl -X POST http://localhost:8080/api/process/gime404/start \
     -d '{"key":"(id,007)", "input":"be careful with that customer", "requestor":"call_center"}' \
     -i \
     --header 'Content-Type: application/json'
