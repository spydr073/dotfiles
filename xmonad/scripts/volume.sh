#!/usr/bin/env bash

if [[ $1 = "get" ]]
then
  if [[ "$(pamixer --get-mute)" = "true" ]]; then
    echo "$(pamixer --get-volume)*"
  else
    echo "$(pamixer --get-volume)"
  fi
elif [[ $1 = "up" ]]
then
  for SINK in `pamixer --list-sinks | awk 'FNR==2 {print $1}'`
  do
    pamixer --sink $SINK -i 2
  done
elif [[ $1 = "down" ]]
then
  for SINK in `pamixer --list-sinks | awk 'FNR==2 {print $1}'`
  do
    pamixer --sink $SINK -d 2
  done
elif [[ $1 = "mute" ]]
then
  for SINK in `pamixer --list-sinks | awk 'FNR==2 {print $1}'`
  do
    if [[ "$(pamixer --get-mute)" = "true" ]]
    then
      pamixer --sink $SINK -u
    else
      pamixer --sink $SINK -m
    fi
  done
else
    echo "Usage: set-vol [up|down|mute]"
    exit 2
fi


