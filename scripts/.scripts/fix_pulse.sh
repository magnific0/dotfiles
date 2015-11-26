#!/bin/sh
pulseaudio --kill
killall pulseaudio
killall -9 pulseaudio
rm -rf ~/.pulse*
rm -rf ~/.config/pulse/*
rm -rf /tmp/pulse*
pulseaudio --start
