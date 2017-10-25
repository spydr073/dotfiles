#!/run/current-system/sw/bin/bash

flag=false
case $1 in
  "up")
    action="volume"
    cmd="+2%"
    ;;
  "down")
    action="volume"
    cmd="-2%"
    ;;
  "mute")
    action="mute"
    cmd="toggle"
    ;;
  "get")
    flag=true
    ;;
  *)
    echo "Usage: set-vol [up|down|mute]"
    exit 2
    ;;
esac

default=$(pactl list short sinks | grep "RUNNING" | cut -f1)
#default=$(pactl info | grep "Default Sink" | cut -f2 -d: | sed 's/^ *//')

if $flag ; then
  echo "$(pamixer --sink $default --get-volume)"
else
  pactl set-sink-$action $default $cmd
fi


