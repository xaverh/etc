export TERMINAL=alacritty
# https://github.com/alacritty/alacritty/issues/1501
export WINIT_HIDPI_FACTOR=1
export XCURSOR_THEME="DMZ-White"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -configdir .config/X11/xorg.conf.d/"$HOSTNAME"/ -keeptty -nolisten tcp >| ~/.cache/.xorg.log 2>&1
