#!/bin/bash

[[ -f $HOME/.dmenurc ]] && . $HOME/.dmenurc || DMENU='dmenu -i'

DEV_LABEL="/dev/disk/by-label/"
TMP="/tmp/dmnt-udevil-$(date +%s)"

trap "rm -f $TMP" EXIT

opt_mount_type=0
opt_ignore_filter=0
opt_notify=0
udevil_cmd="mount"

usage() {
	cat <<-EOF
		usage: dmenu-udevil [-mudhins]
		 -m Mount devices
		 -u Unmount devices
		 -s Mount special
		 -d Select by device rather than by label
		 -i Ignore filter and list all devices in /dev (with -d)
		 -n Pass udevil output to notify-send
		 -h Print help
	EOF

}

dmenu_mnt() {
	if [[ $opt_mount_type -eq 1 ]]; then
		prompt="$udevil_cmd by-device:"
		if [ $opt_ignore_filter -eq 0 ]]; then
			res=$(find /dev -maxdepth 1 -not -type d -name "s[dr]*" -or -name "hd*" | cut -d'/' -f3)
		else
			res=$(find /dev -maxdepth 1 -not -type d | cut -d'/' -f3)
		fi
		res="$(echo $res  | ${DMENU} -p "$prompt")"

		path="/dev/$res"

		[[ -z $res ]] && echo "Cancelled." && exit
	else
		prompt="$udevil_cmd by-label:"
		res="$(find $DEV_LABEL* | cut -d'/' -f5 | ${DMENU} -p "$prompt")"

		path="$DEV_LABEL/$res"

		[[ -z $res ]] && echo "Cancelled." && exit
	fi

}

dmenu_mnts() {

    res="$(gpg -d ~/.config/dmenu-udevil/specialmounts.gpg | ${DMENU} -p "$prompt")"
    path="$res"

}

dmenu_umnt() {
	
	res="$(ls -1d /media/* | cut -d'/' -f3 | ${DMENU} -p "$prompt")"
	path="/media/$res"
	[[ -z $res ]] && echo "Cancelled." && exit

}

while getopts ':mudhins' opt; do
	case "$opt" in
	    m) 
		dmenu_mnt
		;;
	    u) 	
		udevil_cmd="umount"
		dmenu_umnt
		;;
	    s)
		dmenu_mnts
		;;
	    d) opt_mount_type=1;;
	    i) opt_ignore_filter=1;;
	    h) usage && exit;;
	    n) opt_notify=1;;
	    /?) echo "Unrecognized command: $OPTARG";;
	esac
done

[[ -z $res ]] && echo "Cancelled." && exit

udevil $udevil_cmd "$path" > "$TMP" 2>&1
exitc=$?

if [[ $opt_notify -eq 1 ]]; then
    case $exitc in
	0) urgency="normal";;
	*) urgency="critical";;
    esac
    notify-send -u $urgency "$(<$TMP)"
else
    cat "$TMP"
fi			

exit
