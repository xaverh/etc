#!/bin/zsh

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
# Add your own lines here...
#


# day planer deamon
# pland &

xset b off &
xset s off &
xset -dpms &

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# xsetroot -bitmap $HOME/etc/backdrops/`ls $HOME/etc/backdrops | shuf -n 1 | tr -d '\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`

# xsetroot -solid "#57655a"

# ~/.fehbg &

# numlockx &

urxvt-mld -q -o -f

# sxhkd -m -1 &
# exec bspwm
# exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
# exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
mkdir /tmp/fvwmout
exec fvwm >> /tmp/fvwmout/stdout 2>> /tmp/fvwmout/stderr

# call failsafe
exit 0

