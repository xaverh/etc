export TERMINAL=alacritty
# https://github.com/alacritty/alacritty/issues/1501
export WINIT_HIDPI_FACTOR=1
export XCURSOR_THEME="DMZ-White"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -config $HOME/.config/X11/xorg.conf."$HOSTNAME" -keeptty -nolisten tcp
