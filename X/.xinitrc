#!/bin/bash

# Sample .xinitrc for SuSE Linux
# This script is called from 'startx' when you start an X session
#

#
# In case everything goes wrong, we at least fall back to a plain xterm
#
failsafe="xterm -ls -T Failsafe -geometry 80x24-0-0"
trap "exec $failsafe" EXIT SIGHUP SIGINT SIGPIPE SIGTERM SIGIO

#
# Some bash (1 and 2) settings to avoid trouble on a
# failed program call.
#
set +e > /dev/null 2>&1
set +u > /dev/null 2>&1
set +o posix  > /dev/null 2>&1
if type shopt > /dev/null 2>&1 ; then
    shopt -s execfail
else
    no_exit_on_failed_exec=1
fi

#
# Source common code shared between the
# X session and X init scripts
#
. /etc/X11/xinit/xinitrc.common

#
# Uncomment next lines to activate asking for ssh passphrase
#
# if test -S "$SSH_AUTH_SOCK" -a -x "$SSH_ASKPASS"; then
#       ssh-add < /dev/null
# fi

#
# Add your own lines here...
#


# day planer deamon
# pland &

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

xsetroot -bitmap $HOME/etc/backdrops/`ls $HOME/etc/backdrops | shuf -n 1 | tr -d '\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`

# ~/.fehbg &

numlockx &

# urxvtd -q -o -f

# urxvt -name journalctl -e tmux new-session -A -s journalctl "journalctl -b -f -n 1000" &

# (sleep 3 && jigglyroom 0 | lemonbar -a 11 -g x18 -f "-sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1" -f "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso10646-1"  -B "#1E1E1E" -F "#E5E6E6" | sh) &

#while true; do
    #2bwm >/dev/null 2>&1
#done

exec herbstluftwm --locked

# call failsafe
exit 0

