#!/usr/bin/zsh

monitor=${1:-0}
geometry=( $(herbstclient monitor_rect "$monitor") )
if [ -z "$geometry" ] ;then
    echo "Invalid monitor $monitor"
    exit 1
fi
# geometry has the format W H X Y
x=${geometry[0]}
y=${geometry[1]}
panel_width=${geometry[2]}
panel_height=18
# bgcolor=$(hc get frame_border_normal_color)
# selbg=$(hc get window_border_active_color)
selfg='#101010'

herbstclient pad $monitor $panel_height

$HOME/etc/suckless/schwammerl/schwammerl $monitor 2> /dev/null | lemonbar -g x${panel_height} -f -sgi-screen-medium-r-normal--12-120-72-72-m-70-iso8859-1 -f -xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso10646-1 -B '#1e1e1e' -F '#f9f8f4' | /usr/bin/zsh