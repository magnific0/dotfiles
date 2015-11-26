#!/bin/sh
/usr/bin/pacaur -qSy 1>/dev/null 2>&1
echo $(/usr/bin/pacaur -rQu | wc -l)" "$(/usr/bin/pacaur -aQu | wc -l) > /tmp/pacman.count
