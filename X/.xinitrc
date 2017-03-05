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

~/.fehbg &
xset b off &
xset s off &
xset -dpms &

SSH_ENV=$HOME/.ssh/environment
ssh-agent | grep -v echo > ${SSH_ENV}
chmod 600 ${SSH_ENV}
. ${SSH_ENV} > /dev/null

if test -f /usr/lib/ssh/x11-ssh-askpass
then
	SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass ssh-add < /dev/null
fi

dunst &

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &

systemctl start --user dwmstatus.service

while true; do
    # Log stderror to a file
    # dwm 2> ~/.dwm.log
    # No error logging
    dwm >/dev/null 2>&1
done
