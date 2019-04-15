#!/usr/bin/zsh

monitor=${1:-0}

panel_height=19
font="-sgi-screen-medium-r-normal--14-140-72-72-m-70-iso8859-1"
font2="-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1"
# bgcolor=$(herbstclient get frame_border_normal_color)
# selbg=$(herbstclient get window_border_active_color)

herbstclient pad $monitor $panel_height

jigglyroom "$monitor" | lemonbar -u 2 -g x$panel_height -a 12 -n herbstluft_panel -f "$font" -f "$font2" -F "#E5E6E6" -B "#1E1E1E" | /usr/bin/zsh &
