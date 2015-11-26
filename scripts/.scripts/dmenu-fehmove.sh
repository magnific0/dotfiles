#!/bin/bash

[[ -f $HOME/.dmenurc ]] && . $HOME/.dmenurc || DMENU='dmenu -i'


topdir="$HOME/Images/"
while true; do
    res=$(ls -d $topdir*/ | sed "s/$(echo $topdir |sed -e 's/[\/&]/\\&/g')//g")
    res="$(echo -e ".\n$res" | ${DMENU} -p "$prompt")"

if [[ -z $res ]]; then
    echo "Cancelled."
    exit
fi
if [[ $res = "." ]]; then
    mv "$1" "$topdir"
    exit
fi
topdir="$topdir$res/"
if [[ $(find $topdir -mindepth 1 -maxdepth 1 -type d | wc -l) -eq 00 ]]; then
    mv "$1" "$topdir"
    exit
fi
done

