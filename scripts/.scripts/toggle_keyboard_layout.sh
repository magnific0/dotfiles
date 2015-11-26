#!/bin/bash
if [ -f /tmp/usactive ]; then
 setxkbmap -layout "us" -variant "dvorak"
 rm /tmp/usactive
else
 setxkbmap -layout "us" -variant "altgr-intl"
 touch /tmp/usactive
fi
/home/wacko/.scripts/S2C/S2C.sh &
