#!/bin/sh
message=$(echo $2 | awk '{$1=$2=""; print $0}')
echo "naughty.notify({title = \"udevil:\", text =\"$message\", timeout = 10})" | ~/.scripts/myawesomeclient.sh
exit 0
