#!/usr/bin/zsh

pidof lemonbar && kill `pidof lemonbar`
for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
	local panel_height=17
	geometry=( $(herbstclient monitor_rect "$monitor") )
	x=${geometry[1]}
	y=${geometry[2]}
	width="${geometry[3]}"
	(schwammerl "$monitor" | lemonbar -u 2 -g "$(printf '%dx%d%+d%+d' $width $panel_height $x $y)" -f -uw-ttyp0-medium-r-normal--14-130-75-75-c-70-iso10646-1 -f "-wenquanyi-wenquanyi bitmap song-medium-r-normal--12-120-75-75-p-119-iso10646-1"   -a 17 -B "$QI_W" -F "$1" -U "$1" | $SHELL) &
	herbstclient pad "$monitor" "$panel_height"
done
