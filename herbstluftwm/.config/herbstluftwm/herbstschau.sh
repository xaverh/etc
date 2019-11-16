#!/usr/bin/zsh

function external_right() {
cat <<EOF
┌───────┐ ┏━━━━┓
│   I   │ ┃ II ┃   2nd monitor to the right
│       │ ┗━━━━┛
└───────┘
EOF
}

function external_left() {
cat <<EOF
 ┏━━━━┓ ┌───────┐
 ┃ II ┃ │   I   │  2nd monitor to the left
 ┗━━━━┛ │       │
        └───────┘
EOF
}

function external_off() {
cat <<EOF
┌───────┐ ╭┄┄┄┄╮
│   II  │ ┆ ☠️ ┆   2nd monitor disabled
│       │ ╰┄┄┄┄╯
└───────┘
EOF
}

function internal_off() {
cat <<EOF
╭┄┄┄┄┄┄┄╮ ┏━━━━┓
┆  ☠️   ┆ ┃ II ┃   1st monitor disabled
┆       ┆ ┗━━━━┛
╰┄┄┄┄┄┄┄╯
EOF
}


function print_menu() {
	external_left
	echo -e '\0'
	external_right
	echo -e '\0'
	external_off
	echo -e '\0'
	internal_off
}

element_height=5
element_count=4

res=$(print_menu | rofi -dmenu -sep '\0' -lines "$element_count" -eh "$element_height" -p '' -no-custom -format d)

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
