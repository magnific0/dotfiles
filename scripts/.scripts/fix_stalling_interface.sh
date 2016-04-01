#!/bin/sh
# Original ifconfig version by /u/jp599 with minor comments by /u/KnowsBash
# https://www.reddit.com/r/commandline/comments/43ytzb/dealing_with_a_crap_wireless_router/
rx_packets () {
  ip -s link show wlp6s0 | awk '/RX:/{ getline; print $2 }'
}
main () {
  local interface="$1"
  local rx1=0
  local rx2
  while true; do
    rx2=$(rx_packets "$interface")
    printf "rx=%d\n" "$rx2"
    if [[ "$rx1" -eq "$rx2" ]]; then
      date
      ip link set "$interface" down
      sleep 3
      ip link set "$interface" up
    fi
    rx1="$rx2"
    sleep 15
  done
}
main "$@"
