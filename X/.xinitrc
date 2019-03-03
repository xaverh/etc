#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
	xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
	xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
	xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
	xmodmap "$usermodmap"
fi

xsetroot -solid "#1e1e1e"
~/.fehbg &
xset b off &
xset s off &
xset -dpms &

# if test -f /usr/lib/ssh/x11-ssh-askpass
# then
# 	SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass ssh-add < /dev/null
# fi &

(sleep 5 && dunst)&

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &

# systemctl --user start dwmstatus.service &

st -t journalctl -e tmux new-session -s journalctl "journalctl -b -f -n 1000" &

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# while true; do
    # Log stderror to a file
    # dwm 2> ~/.dwm.log
    # No error logging
    # dwm >/dev/null 2>&1
# done

exec notion 2> ~/.notion.log

