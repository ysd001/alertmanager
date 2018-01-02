#!/bin/bash
for i in $(seq 1 10000); do
  ./amtool silence add -c 'test' -e '1h' --alertmanager.url 'http://localhost:9093' 'node=bar'$i
done
