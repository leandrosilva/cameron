#!/bin/sh

# for benchmark:
#     ab -n 10000 -c 50 -p benchmark_request.json http://localhost:8080/api/diagnostic/ask

curl -X POST http://localhost:8080/api/diagnostic/ask \
     -d '{"customer_id":"007", "from_id":"call_center"}' \
     -i --header 'Content-Type: application/json'
     