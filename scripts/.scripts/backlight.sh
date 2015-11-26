#!/bin/bash
MAXBACKLIGHT=$(cat /sys/class/backlight/intel_backlight/max_brightness)
CURBACKLIGHT=$(expr $(cat /sys/class/backlight/intel_backlight/brightness) \* 100 / $MAXBACKLIGHT)
if [ -n "$1" ]; then
    if [[ $1 == \+[0-9]* ]]; then
        NEWBACKLIGHT=$(expr \( $CURBACKLIGHT + ${1//[!0-9]/} \) \* $MAXBACKLIGHT / 100)
        if [ $NEWBACKLIGHT -ge $MAXBACKLIGHT ]; then
            NEWBACKLIGHT=$MAXBACKLIGHT
        fi
    elif [[ $1 == -[0-9]* ]]; then
        NEWBACKLIGHT=$(expr \( $CURBACKLIGHT - ${1//[!0-9]/} \) \* $MAXBACKLIGHT / 100)
        if [ $NEWBACKLIGHT -le "0" ]; then
            NEWBACKLIGHT=0
        fi
    elif [ "$1" -le "100" ] && [ "$1" -ge "0" ]; then
        NEWBACKLIGHT=$(expr ${1//[!0-9]/} \* $MAXBACKLIGHT / 100)
    fi
    #if [  "$1" -eq "100" ]; then
    #echo "Tip: Leave away the argument to maximum brightness."
    #fi
    sudo tee /sys/class/backlight/intel_backlight/brightness <<< $NEWBACKLIGHT
else
    echo $CURBACKLIGHT
fi

