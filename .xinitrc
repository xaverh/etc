#!/bin/sh

# xsetroot -solid "#1e1e1e"
xset b off &
xset s off &
xset -dpms &

# if test -f /usr/lib/ssh/x11-ssh-askpass
# then
# 	SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass ssh-add < /dev/null
# fi &

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

exec herbstluftwm 2> /tmp/herbstluftwm.log
