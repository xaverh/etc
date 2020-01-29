#!/usr/bin/zsh

pidof lemonbar && kill `pidof lemonbar`
for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
	local panel_height=16
	geometry=( $(herbstclient monitor_rect "$monitor") )
	x=${geometry[1]}
	y=${geometry[2]}
	width="${geometry[3]}"
	(schwammerl "$monitor" | lemonbar -g "$(printf '%dx%d%+d%+d' $width $panel_height $x $y)" -f -uw-ttyp0-medium-r-normal--14-130-75-75-c-70-iso10646-1 -f "-wenquanyi-wenquanyi bitmap song-medium-r-normal--12-120-75-75-p-119-iso10646-1" -f "-fontforge-al abdali 8-regular-r-normal--8-80-75-75-p-63-iso10646-1" -a 18 -B "$QI_W" -F "$QI_B_K" | $SHELL ) 2> /tmp/herbstlog &
	herbstclient pad "$monitor" "$panel_height"
done
