#!/bin/zsh

#
# In case everything goes wrong, we at least fall back to a plain xterm
#
failsafe="xterm -ls -T Failsafe -geometry 80x24-0-0"
trap "exec $failsafe" EXIT SIGHUP SIGINT SIGPIPE SIGTERM SIGIO

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

# https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Key_combinations
export GTK_IM_MODULE="xim"
export XMODIFIERS="@im=none"

# xsetroot -bitmap $HOME/etc/backdrops/`ls $HOME/etc/backdrops | shuf -n 1 | tr -d '\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`

# xsetroot -solid "#57655a"

numlockx &

go run $HOME/etc/suckless/jigglyroom/jigglyroom.go &

urxvtd -q -o -f

(sleep 3 && urxvtc -name Journalctl -e tmux new-session -A -s journalctl "journalctl -b -f -n 1000") &

while true; do
    # Log stderror to a file
    dwm 2> ~/.dwm.log
    # No error logging
    #dwm >/dev/null 2>&1
done

# call failsafe
exit 0

