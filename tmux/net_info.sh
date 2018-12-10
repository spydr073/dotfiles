#!/bin/sh

WLAN=$(ifconfig wlp5s0 | grep 'inet ' | awk '{print $2}')
ETH0=$(ifconfig enp3s0 | grep 'inet ' | awk '{print $2}')
PUB_IP=$(dig +short myip.opendns.com @resolver1.opendns.com)
MAC=$(cat /sys/class/net/$(ip route show default | awk '/default/ {print $5}')/address)

LOC_IP=""
if [[ "$WLAN" != "" && "$ETH0" != "" ]]; then
	LOC_IP="W:$WLAN E:$ETH0"
elif [[ "$WLAN" != "" ]]; then
	LOC_IP="$WLAN"
elif [[ "$ETH0" != "" ]]; then
	LOC_IP="$ETH0"
fi

echo "#[fg=#777777,bg=#222222] ${MAC} #[default] #[fg=#777777,bg=#222222] ${WLAN} #[default] #[fg=#777777,bg=#222222] ${PUB_IP} #[default]"

