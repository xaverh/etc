#!/bin/sh

xsetroot -solid "#1e1e1e"
xset b off &
xset s off &
xset -dpms &

# if test -f /usr/lib/ssh/x11-ssh-askpass
# then
# 	SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass ssh-add < /dev/null
# fi &

(sleep 2 && dunst)&

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &

st -t journalctl -e tmux new-session -s journalctl "journalctl -b -f -n 1000" &

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

urxvtd -q -o -f &

exec bspwm 2> /tmp/bspwm.log
