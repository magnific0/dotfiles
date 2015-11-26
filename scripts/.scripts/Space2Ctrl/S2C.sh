#! /bin/sh

# 37 Control_L					# May Differ for your System. Use 'xev' to get the right codes
# 65 space
# 105 Control_R
# 64 Alt_L
# 108 Alt_R (ISO_Level3_Shift)
# 66 Caps_Lock      
# 133 Left Windows Flag
# 135 Right Menu

if [ ! -f ~/.scripts/Space2Ctrl/xmodmap.orig ]
then
xmodmap -pke > ~/.scripts/Space2Ctrl/xmodmap.orig
fi

#xmodmap -e "clear Lock"				
#xmodmap -e "keycode 66 = Meta_L"		# Caps Lock = Meta key
#xmodmap -e "add Mod1 = Meta_L"

xmodmap -e "keycode 65 = Control_L"     	# Space bar = Left control key
xmodmap -e "add control = Control_L"

~/.scripts/Space2Ctrl/Space2Ctrl
