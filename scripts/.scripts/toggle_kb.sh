#!/bin/sh
KB_LAY[0]="us"
KB_LAY[1 ]="dvorak"
KB_LEN=${#KB_LAY[@]}

[[ -f ${HOME}/.dmenurc ]] && . ${HOME}/.dmenurc || DMENU='dmenu -i'

KB_LST=$(xinput list | grep -n keyboard | grep -vi button | grep -vi hotkeys | \
         grep -vi bus | grep -vi webcam | awk -F: '{ print $1 }')
KB_END=$(echo "${KB_LST}" | tail -1 | awk -F: '{ print $1 }')
KB_NAM=$(echo "${KB_LST}" | xargs -I{} sh -c "xinput list --name-only | sed -n -e {}p")
KB_IDS=$(echo "${KB_LST}" | xargs -I{} sh -c "xinput list --id-only | sed -n -e {}p")

# Get choice from DMENU, exit if DMENU is quit improperly (e.g. ESC)
res=$(echo "${KB_NAM}" | ${DMENU} -p "Select keyboard") || exit $?

KB_SID=$(echo "${KB_NAM}" | grep -n "${res}" | head -1 | awk -F: '{ print $1 }')

KB_EID=$(echo "${KB_IDS}" | awk "NR==${KB_SID}") 

if [ -n "$KB_EID" ]; then
    KB_LOC=$(setxkbmap -query -device ${KB_EID} | grep layout | awk '{print $2}' | awk -F, '{print $1}')
    for i in "${!KB_LAY[@]}"; do
        if [ "${KB_LAY[$i]}" == "${KB_LOC}" ] ; then
            KB_LON=${KB_LAY[$((($i+1)%${KB_LEN}))]}
            echo "Changing keyboard layout from $KB_LOC to $KB_LON"
            setxkbmap -device ${KB_EID} ${KB_LON}
        fi
    done
    setxkbmap -device ${KB_EID} -option compose:caps
else
    echo "ErgoDox not found!"
fi
