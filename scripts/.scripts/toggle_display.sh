#!/bin/bash
# Get number of monitors
xrandr --newmode "1920x1080_60.00"  172.80  1920 2040 2248 2576  1080 1081 1084 1118  -HSync +Vsync
xrandr --addmode DP1 1920x1080_60.00

OINT="eDP1"
OEXT="DP1"
MINT="1920x1080"
MEXT="1920x1080_60.00"

CNOOFM=`xrandr --verbose | grep -c CRTC:`
if [ $CNOOFM -eq 1 ]
then
  # If only one display is connected get ID of display connected
  CMID=`xrandr --verbose | grep CRTC: | awk -F' ' '{print $(NF)}'`
  if [ $CMID -eq 0 ]
  then
    echo "Switch to VGA only"
    XRARGS="--output $OEXT --mode $MEXT --output $OINT --off"
  else
    echo "Switch to Clone"
    XRARGS="--output $OINT --mode %MINT --output $OEXT --mode $MEXT --right-of $OINT"
  fi
else
  echo "Switch to Internal only"
  XRARGS="--output $OEXT --off"
fi
echo "xrandr $XRARGS"
xrandr $XRARGS
