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

urxvtd -q -o -f

urxvtc -name journalctl -e tmux new-session -A -s journalctl "journalctl -b -f -n 1000" &

(sleep 3 && /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1) &

(test -x ~/.fehbg && ~/.fehbg) &

(sleep 2 && dunst)&

# go run bin/bspwmstatus.go | lemonbar -name jigglyroom -f "Iosifovich:style=Medium,Regular:pixelsize=16:antialias=true:hintstyle=1:lcdfilter=1:rgba=1" -B "#1E1E1E" -F "#E5E6E6" | sh &

exec herbstluftwm 2> /tmp/bspwm.log
