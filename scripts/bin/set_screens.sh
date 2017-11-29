#!/bin/sh

choice=0
while [[ "$choice" -lt 1 || "$choice" -gt 3 ]]; do
    echo ""
    echo "Select layout:"
    echo "  1) home"
    echo "  2) work"
    echo "  3) mobile"

    read choice

    case $choice in

      "1") xrandr --output eDP1  --mode 1920x1080 --rotate normal --left-of HDMI1
           xrandr --output HDMI1 --mode 1920x1080 --rotate normal --left-of DP1    --right-of eDP1
           xrandr --output DP1   --mode 1920x1080 --rotate normal                  --right-of HDMI1
           ;;

      "2") xrandr --output DP1   --mode 1920x1080 --rotate normal --left-of HDMI1
           xrandr --output HDMI1 --mode 1920x1080 --rotate normal --left-of eDP1   --right-of DP1
           xrandr --output eDP1  --mode 1920x1080 --rotate normal                  --right-of HDMI1
           ;;

      "3") xrandr --output eDP1  --mode 1920x1080 --rotate normal
           ;;

      *) echo ""
         echo "invalid choice, try again..."

    esac
done


