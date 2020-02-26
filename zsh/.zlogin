[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx "${XDG_CONFIG_HOME:-$HOME/.config}/xinitrc" -- -config "${XDG_CONFIG_HOME:-$HOME/.config}/X11/xorg.conf.$HOSTNAME" -keeptty -nolisten tcp
