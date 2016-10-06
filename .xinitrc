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

compton --config ~/.config/compton.conf --daemon &
~/.fehbg &
xset b off &
xset s off &
xset -dpms &

urxvtd --fork --quiet --opendisplay &
dunst &

# This should be in the accoring config file
if egrep -iq 'touchpad' /proc/bus/input/devices; then
	synclient VertEdgeScroll=0 HorizEdgeScroll=0 TapButton1=1 VertScrollDelta=-45 HorizScrollDelta=-45 PalmDetect=1 2>/dev/null
	syndaemon -i 0.5 -t -K -R -d &
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &

dwm_status &

exec dwm
