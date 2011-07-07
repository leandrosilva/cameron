#!/bin/sh

curl -X POST http://localhost:8080/api/diagnostic/ask \
     -d '{"customer_id":"007", "from_id":"call_center"}' \
     -i --header 'Content-Type: application/json'
     