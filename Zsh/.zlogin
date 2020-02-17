export TERMINAL=alacritty
# https://github.com/alacritty/alacritty/issues/1501
export WINIT_HIDPI_FACTOR=1
export XCURSOR_THEME="DMZ-White"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp >| ~/.xorg.log 2>&1
export IS_YSGRIFENNWR=0
