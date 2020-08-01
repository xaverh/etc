#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx "${XDG_CONFIG_HOME:-$HOME/.config}/xinitrc"
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sway

