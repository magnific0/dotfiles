#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# GDK 3 (GTK+ 3) HiDPI fix
# To scale UI elements by a factor of two: 
# export GDK_SCALE=2
# To undo scaling of text:
# export GDK_DPI_SCALE=0.5

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
feh --bg-fill ~/bg.jpg

# Add fucking display modes because shitty intel/xorg refuses to detect it properly
# gtf 1920 1080 60
xrandr --newmode "1920x1080_60.00"  172.80  1920 2040 2248 2576  1080 1081 1084 1118  -HSync +Vsync
xrandr --addmode DP1 1920x1080_60.00
xrandr --output DP1 --mode 1920x1080_60.00
