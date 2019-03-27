#!/bin/sh

BARBG=$1
TAGBG=$2
TAGFG=$3

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

MACSTR="#[fg=${TAGBG}]#[bg=${BARBG}]\
#[fg=${TAGFG}]#[bg=${TAGBG}] ${MAC} \
#[fg=${BARBG}]#[bg=${TAGBG}]\
#[default]"

LOCSTR="#[fg=${TAGBG}]#[bg=${BARBG}]\
#[fg=${TAGFG}]#[bg=${TAGBG}] ${LOC_IP} \
#[fg=${BARBG}]#[bg=${TAGBG}]\
#[default]"

IPSTR="#[fg=${TAGBG}]#[bg=${BARBG}]\
#[fg=${TAGFG}]#[bg=${TAGBG}] ${PUB_IP} \
#[default]"

# 
echo "${MACSTR}${LOCSTR}${IPSTR}"

