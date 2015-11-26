
#!/bin/sh
max_res=$(xrandr | grep VGA1 -A 1 | tail -1 | awk '{ print $1 };')

# RESOLUTION SETTINGS
# This sets your VGA1 monitor to its best resolution.
xrandr --output VGA1 --mode $max_res --rate 60
# This sets your laptop monitor to its best resolution.
xrandr --output LVDS1 --mode 1600x900 --rate 60

# MONITOR ORDER
# Put the Laptop right, VGA1 monitor left
# xrandr --output VGA1 --left-of LVDS1
# Put the Laptop left, VGA1 monitor right
xrandr --output VGA1 --right-of LVDS1

# PRIMARY MONITOR
xrandr --output LVDS1 --primary
# xrandr --output VGA1 --primary
# This sets your laptop monitor as your primary monitor.
# This sets your VGA monitor as your primary monitor.
# xrandr --output VGA1 --primary
