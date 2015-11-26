#!/bin/sh
source /home/wacko/.dmenurc || $dmenu_style = ""
select_and_connect() {
    blserv=`grep "." /home/wacko/.config/blacklogic.servers | dmenu -p ">" -i -l 10 $dmenu_style | awk ' {print $NF} '`
    if [ -z  $blserv ]; then
	exit 1
    fi
    echo "pty \"pptp $blserv --nolaunchpppd\"" | tee /etc/ppp/peers/blacklogic
    cat /etc/ppp/peers/blacklogic.empty | tee -a /etc/ppp/peers/blacklogic
    pon blacklogic updetach persist > /dev/null 2>&1 &
    ppp0test=`ifconfig | grep -o ppp0`
    tries=0
    while [ "ppp0" != "$ppp0test" ] && [ "$tries" != "12" ]; do
	sleep 5
	tries=`expr $tries + 1`
	ppp0test=`ifconfig | grep -o ppp0`
    done
}

tries=12
while [ $tries = "12" ]; do
    select_and_connect
done

