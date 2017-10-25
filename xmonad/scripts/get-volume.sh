#!/run/current-system/sw/bin/bash

vol=$(amixer -c 0 get Master | egrep -o "[0-9]+%")
echo "${vol%?}"


