#!/bin/bash

REDIS_PORT=6379

while true; do
  result=$(netstat -lnp 2> /dev/null | grep $REDIS_PORT)

  if [ -z "$result" ]; then
    nohup redis_ocaml_server "$@" > /dev/null 2>&1 &
    sleep 0.5 # to be sure that server is running
    break
  fi

  # Sleep between iterations to avoid overwhelming the system
  sleep 0.2
done
