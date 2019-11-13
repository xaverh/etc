#!/usr/bin/zsh

# https://www.uninformativ.de/blog/postings/2018-06-02/0/POSTING-de.html
# [[ "$1" != logging ]] && exec systemd-cat "$0" logging

#
# In case everything goes wrong, we at least fall back to a plain xterm
#
failsafe="xterm -ls -T Failsafe -geometry 80x24-0-0"
trap "exec $failsafe" EXIT SIGHUP SIGINT SIGPIPE SIGTERM SIGIO

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [[ -f $sysresources ]]; then
	xrdb -merge $sysresources
fi

if [[ -f $sysmodmap ]]; then
	xmodmap $sysmodmap
fi

if [[ -f $userresources ]]; then
	xrdb -merge "$userresources"
fi

if [[ -f $usermodmap ]]; then
	xmodmap "$usermodmap"
fi

if [[ -f /etc/X11/xinit/xinitrc.common ]] ; then
	. /etc/X11/xinit/xinitrc.common
fi

if [[ -d /etc/X11/xinit/xinitrc.d ]] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[[ -x $f ]] && . "$f"
	done
	unset f
fi

xset b off &
xset s off &
xset -dpms &


# https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Key_combinations
export GTK_IM_MODULE="xim"
export XMODIFIERS="@im=none"

xsetroot -bitmap $HOME/.local/share/backdrops/`ls $HOME/.local/share/backdrops | shuf -n 1 | tr -d '\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`

([[ -x /usr/bin/numlockx ]] && numlockx) &

exec herbstluftwm --locked 2> /tmp/herbstlog

# call failsafe
exit 0

