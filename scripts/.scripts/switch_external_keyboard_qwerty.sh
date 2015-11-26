#!/bin/bash
EDID=`xinput -list | grep "DELL Dell USB Entry Keyboard" | tr '\t' '\n' | grep id | awk -F"=" '{ print $2}'`
setxkbmap -device $EDID us
