#!/usr/bin/zsh

# xset b off &
# xset s off &
# xset -dpms &
# https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Key_combinations
# export GTK_IM_MODULE="xim"
# export XMODIFIERS="@im=none"
# xsetroot -bitmap $HOME/.local/share/backdrops/`\\ls $HOME/.local/share/backdrops | shuf -n 1 | tr -d '\\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`

# mega-login &!

# exec awesome 2>>| ~/.cache/awesome.log
exec awesome
