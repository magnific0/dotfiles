#!/usr/bin/bash
wpa_supplicant -B -i wlan1 -c ~/.scripts/adhocphone.conf -D nl80211,wext
