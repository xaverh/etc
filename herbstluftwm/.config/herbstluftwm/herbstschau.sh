#!/usr/bin/zsh

function external_right() {
	<<-EOF
	┌───────┐ ┏━━━━┓
	│   I   │ ┃ II ┃   2nd monitor to the right
	│       │ ┗━━━━┛
	└───────┘
	EOF
}

function external_left() {
	<<-EOF
	 ┏━━━━┓ ┌───────┐
	 ┃ II ┃ │   I   │  2nd monitor to the left
	 ┗━━━━┛ │       │
	        └───────┘
	EOF
}

function external_off() {
	<<-EOF
	┌───────┐ ╭┄┄┄┄╮
	│   II  │ ┆ ☠️ ┆   2nd monitor disabled
	│       │ ╰┄┄┄┄╯
	└───────┘
	EOF
}

function internal_off() {
	<<-EOF
	╭┄┄┄┄┄┄┄╮ ┏━━━━┓
	┆  ☠️   ┆ ┃ II ┃   1st monitor disabled
	┆       ┆ ┗━━━━┛
	╰┄┄┄┄┄┄┄╯
	EOF
}


function print_menu() {
	external_left
	print -n '\0'
	external_right
	print -n '\0'
	external_off
	print -n '\0'
	internal_off
}

res=$(print_menu | rofi -dmenu -p 'Display layout' -sep '\0' -lines 4 -eh 4 -no-custom -format d)

if [[ -z "$res" ]] ; then
	exit
fi

echo $res

case "$res" in
1)
	mons -e left
	;;
2)
	mons -e right
	;;
3)
	mons -o
	;;
4)
	mons -s
	;;
esac

herbstclient detect_monitors --no-disjoin
$HOME/.config/herbstluftwm/restartpanel.sh `herbstclient get frame_bg_active_color`
