#!/usr/bin/env bash

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
panel_height=23
font="IBM Plex Sans:style=Medium,Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1"
font2="IBM Plex Sans Hebrew Medium:style=Medium,Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1"
font3="IBM Plex Thai Medium:style=Medium,Regular:pixelsize=15:antialias=true:hintstyle=1:lcdfilter=1:rgba=1"
font4="思源黑体 Medium:style=Medium,Regular:pixelsize=14:antialias=true:hintstyle=1:lcdfilter=1:rgba=1"
font5=".SF NS Text:style=Medium:pixelsize=14:antialias=true:hintstyle=1:lcdfilter=1:rgba=1"
bgcolor=$(hc get frame_border_normal_color)
selbg=$(hc get window_border_active_color)

selfg='#101010'

hc pad $monitor $panel_height

# $HOME/src/jigglyroom/jigglyroom "$monitor" | dzen2 -w $panel_width -x $x -y $y -fn "$font" -h $panel_height  -e "button3=;button4=exec:herbstclient use_index -1;button5=exec:herbstclient use_index +1" -ta l -bg "#1e1e1e" -fg "#e5e6e6"
$HOME/src/jigglyroom/jigglyroom "$monitor" | lemonbar -a 32 -u 2 -n herbstluft_panel -g x$panel_height  -f "$font" -f "$font2" -f "$font3" -f "$font4" -f "$font5" -F "#F5F6F6" -B "#1E1E1E" | sh &