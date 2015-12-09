#!/bin/sh
if [ -n "$1" ]; then
    WALLPAPER_MOOD="$1"
else
    WALLPAPER_MOOD="space" 
fi
DIR="$HOME/ownCloud/Images/wallpaper/$WALLPAPER_MOOD/"
PIC=$(ls "$DIR" | shuf -n1)
feh --bg-fill "$DIR/$PIC"
