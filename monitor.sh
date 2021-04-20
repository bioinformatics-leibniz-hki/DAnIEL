#!/usr/bin/env bash

backend_container_id=$(docker ps | grep daniel_backend | cut -f 1 -d " ")

echo "UNIX_TIME" $(docker stats --no-stream | head -n 1)

while true
do
	stat=$(docker stats $backend_container_id --no-trunc --no-stream | tail -n +2)
	echo $EPOCHREALTIME $stat
done

