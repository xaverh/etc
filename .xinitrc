#!/bin/sh

# xsetroot -solid "#1e1e1e"
xset b off &
xset s off &
xset -dpms &

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

## Detect and configure touchpad. See 'man synclient' for more info.
# if egrep -iq 'touchpad' /proc/bus/input/devices; then
#     synclient VertEdgeScroll=1 &
#     synclient TapButton1=1 &
# fi

numlockx &

exec awesome 2> ~/.awesome.err.log
