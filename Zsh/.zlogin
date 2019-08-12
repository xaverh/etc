export TERMINAL=urxvtc
export XCURSOR_THEME="DMZ-White"
# export MOZ_ENABLE_WAYLAND=1

# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp > ~/.xorg.log 2>&1

# exec sway
