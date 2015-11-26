#!/bin/bash
usage()
{
cat << EOF
usage: $0 [-h] [-s <steps>] files(s)

This script will securely delete your files

OPTIONS:
   -h      Show this message
   -s      Number of steps (30 by default)
   -r      Recursively delete (for folders)

EXAMPLES:
$0 my_private_file
$0 my_private_file -s 50

EOF
}

# some defaults
steps=30
fldrd=false

while getopts 'hsr:' o; do
    case "$o" in
	h) 
	    usage
	    exit;;
	s)  
	    steps=$OPTARG
	    ;;
	r)
	    fldrd=true
	    ;;
	[?])
	    usage
	    exit;;
	esac
done
shift `expr $OPTIND - 1`

# different routines for deleting files and folders
if $fldrd; then
    wipe -f -r  -d -n -Z -l2 -x$steps -p$steps -b9 "${@}"
else
    shred -f -z -v -n $steps "${@}"
    wipe -f -d -n -Z -l2 -x32 -p32 -b9 "${@}"
fi

exit
