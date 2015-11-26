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

[ ! -f ~/.xinitrc.$HOSTNAME ] && ~/.xinitrc.$HOSTNAME
