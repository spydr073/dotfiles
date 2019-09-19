#!/usr/bin/env bash

if [[ $# != 4]]
then
  echo "Expected usage: makeColor [red 0-100] [green 0-100] [blue 0-100] [shade 0-255]"
else
  R=$1; G=$2; B=$3; S=$4;
  if [[ $R + $G + $B != 100 ]]
  then
    echo "Invalid ranges! R+G+B must equal 100!"
  else

  fi
fi
