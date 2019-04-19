export PANEL_FIFO=/tmp/panel-fifo
export PANEL_HEIGHT=24
export PANEL_FONT="-sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1"
export PANEL_XFT_FONT="SGI Screen:pixelsize=14"
export PANEL_FALLBACK_FONT="-misc-fixed-medium-r-normal--10-100-75-75-c-60-iso10646-1"
export PANEL_BG_COLOR="#1E1E1E"
export PANEL_FG_COLOR="#F5F6F6"
export PANEL_WM_NAME=bspwm_panel
export TERMINAL=urxvtc

# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp > ~/.xorg.log 2>&1

# (( EUID != 0 )) && umask 0077