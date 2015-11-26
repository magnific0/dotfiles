#!/bin/sh
# v4l2-ctl --list-formats-ext
mplayer tv:// -tv driver=v4l2:width=1280:height=720:device=/dev/video0 -fps 15 -vf screenshot
