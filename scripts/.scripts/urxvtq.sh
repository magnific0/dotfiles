#!/bin/bash

wid=$(xdotool search --classname urxvtq)
if [ -z "$wid" ]; then
  ~/.scripts/urxvtc.sh -bl -depth 32 -bg rgba:3f00/3f00/3f00/dddd -name urxvtq
  wid=$(xdotool search --classname urxvtq | head -1)
  #xdotool windowfocus "$wid"
  #xdotool key Control_L+l
else
  if [ -z "$(xdotool search --onlyvisible --classname urxvtq 2>/dev/null)" ]; then
    xdotool windowmap "$wid"
    xdotool windowfocus "$wid"
  else
    xdotool windowunmap "$wid"
  fi
fi
