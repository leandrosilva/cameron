#!/bin/sh

curl -X POST http://localhost:8080/api/diagnostic/ask \
     -d '{"customer":{"type_id":"login", "value_id": "leandro"}, "from":"call_center"}' \
     -i --header 'Content-Type: application/json'
     