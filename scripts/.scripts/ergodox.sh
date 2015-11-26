#!/bin/bash
EDID=`xinput -list | grep ErgoDox | awk '{print $6}' | awk -F'=' '{print $2}'`
if [ -n "$EDID" ]; then
    setxkbmap -device $EDID us
    setxkbmap -device $EDID -option compose:caps
else
    echo "ErgoDox not found!"
fi
