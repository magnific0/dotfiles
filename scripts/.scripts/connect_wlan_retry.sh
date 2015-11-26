#!/bin/sh
while [ ! "`wicd-cli -y -d | head -1 | awk 'BEGIN { FS = ":" } ; { print $1 }'`" == "IP" -o "`wicd-cli -y -d | head -1 | awk 'BEGIN { FS = " " } ; { print $2 }'`" == "None" ]; do
        EID=`wicd-cli -y -S -l | grep $@ | head | awk 'BEGIN { FS = " " } ; { print $1 }'`
	wicd-cli -y -n $EID -c
	sleep 3
done
