#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

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

# compton --config ~/.config/compton.conf --daemon &
~/.fehbg &
xset b off &
xset s off &
xset -dpms &

dunst &

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &

dwm_status_min &

# syndaemon -i 0.5 -t -K -R &

# tint2 &

while true; do
    # Log stderror to a file
    # dwm 2> ~/.dwm.log
    # No error logging
    dwm >/dev/null 2>&1
done
