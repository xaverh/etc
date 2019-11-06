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
panel_height=20
# bgcolor=$(hc get frame_border_normal_color)
# selbg=$(hc get window_border_active_color)

herbstclient pad $monitor $panel_height

schwammerl $monitor 2> /dev/null | lemonbar -a 13 -g x${panel_height} -f -sgi-screen-medium-r-normal--13-130-72-72-m-70-iso8859-1 -f -misc-fixed-medium-r-normal--13-120-75-75-c-70-iso10646-1 -B '#1e1e1e' -F '#e5e6e6' | /usr/bin/zsh
