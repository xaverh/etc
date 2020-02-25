#!/usr/bin/zsh

# Set up i18n environment
if [ -r /etc/profile.d/lang.sh ]; then
  . /etc/profile.d/lang.sh
fi

[ -r $HOME/.profile ] && . $HOME/.profile

userresources=$HOME/.config/Xresources
usermodmap=$HOME/.config/Xmodmap
userxkbmap=$HOME/.config/Xkbmap

sysresources=/etc/X11/Xresources
sysmodmap=/etc/X11/Xmodmap
sysxkbmap=/etc/X11/Xkbmap

# merge in defaults
[ -r "$sysresources" ] && xrdb -nocpp -merge "$sysresources"
[ -r "$userresources" ] && xrdb -merge "$userresources"

# merge in keymaps
if [ -r "$sysxkbmap" ]; then
    setxkbmap $(cat "$sysxkbmap")
fi

if [ -r "$userxkbmap" ]; then
    setxkbmap $(cat "$userxkbmap")
fi

# xkb and xmodmap don't play nice together
if ! [ -r "$sysxkbmap" -o -r "$userxkbmap" ] ; then
    [ -r "$sysmodmap" ] && xmodmap "$sysmodmap"
    [ -r "$usermodmap" ] && xmodmap "$usermodmap"
fi

# run all system xinitrc shell scripts.
for file in /etc/X11/xinit/xinitrc.d/* ; do
        . $file
done

if [ -x $HOME/.Xclients ]; then
    . $HOME/.Xclients
fi

xset b off &
xset s off &
xset -dpms &
# https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Key_combinations
export GTK_IM_MODULE="xim"
export XMODIFIERS="@im=none"
# xsetroot -bitmap $HOME/.local/share/backdrops/`\\ls $HOME/.local/share/backdrops | shuf -n 1 | tr -d '\\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`

while true; do
	awesome 2>>| ~/.cache/awesome.log
done

