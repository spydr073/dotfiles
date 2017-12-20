#!/run/current-system/sw/bin/bash

flag=false
case $1 in
  "increase")
    action="volume"
    cmd="+2%"
    ;;
  "decrease")
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

if $flag ; then
  if [[ "$(pamixer --get-mute)" = "true" ]]; then
    echo "$(pamixer --get-volume)"
    #echo "_"
  else
    echo "$(pamixer --get-volume)"
  fi
else
  for SINK in `pacmd list-sinks | grep 'index:' | cut -b12-`
  do
    pactl set-sink-$action $SINK $cmd
  done
fi


