#!/usr/bin/env bash

set -x

quote() {
	local q="$(printf '%q ' "$@")"
	printf '%s' "${q% }"
}

hc_quoted="$(quote "${herbstclient_command[@]:-herbstclient}")"
hc() { "${herbstclient_command[@]:-herbstclient}" "$@" ;}
monitor=${1:-0}
geometry=( $(hc monitor_rect "$monitor") )
if [ -z "$geometry" ] ;then
    echo "Invalid monitor $monitor"
    exit 1
fi
# geometry has the format W H X Y
x=${geometry[0]}
y=${geometry[1]}
panel_width=${geometry[2]}
panel_height=20
font="IBM Plex Sans:style=Regular:pixelsize=14:antialias=true:hintstyle=1:lcdfilter=1:rgba=1"
bgcolor=$(hc get frame_border_normal_color)
selbg=$(hc get window_border_active_color)
selfg='#101010'

hc pad $monitor $panel_height

$HOME/lib/jigglyroom/jigglyroom "$monitor" "-ibm-ibm plex sans-medium-i-normal--14-0-0-0-p-0-iso10646-1" $panel_width | dzen2 -w $panel_width -x $x -y $y -fn "$font" -h $panel_height  -e "button3=;button4=exec:herbstclient use_index -1;button5=exec:herbstclient use_index +1" -ta l -bg "#1e1e1e" -fg "#e5e6e6"
