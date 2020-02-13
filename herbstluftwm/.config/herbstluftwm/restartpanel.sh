#!/usr/bin/zsh

pidof lemonbar && kill `pidof lemonbar`
for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
	local panel_height=20
	geometry=( $(herbstclient monitor_rect "$monitor") )
	x=${geometry[1]}
	y=${geometry[2]}
	width="${geometry[3]}"
	(schwammerl "$monitor" | lemonbar -g "$(printf '%dx%d%+d%+d' $width $panel_height $x $y)" -f -gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1 -a 18 -B "$QI_W" -F "$QI_B_K" | $SHELL ) 2> /tmp/herbstlog &
	herbstclient pad "$monitor" "$panel_height"
done
