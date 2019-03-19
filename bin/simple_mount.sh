#/bin/bash

# mount drives via dmenu.
# Inspiration by https://www.youtube.com/watch?v=YOpeXETS2z0

pgrep -x dmenu && exit

mountable="$(lsblk -rpo "name,type,size,mountpoint" | awk '$2=="part"&&$4==""{printf "%s (%s)\n",$1,$3}')"
[[ "$mountable" = "" ]] && exit 1
chosen="$(echo "$mountable" | dmenu -i -p "Mount which drive?" "$@" | awk '{print $1}')"
[[ "$chosen" = "" ]] && exit 1
result="$(udisksctl mount --block-device $chosen)"
[[ "$result" = "" ]] && exit 1
notify-send "$(echo -n $result)"
mountpoint="$(echo -n ${result} | awk '{print $4}')"
mountpoint="${mountpoint::-1}"
program="$(echo -e ":\nurxvtc -e nnn -e\nurxvtc -cd\ncode" | dmenu -p "Where do you want to open ${mountpoint}?" "$@")"
$program $mountpoint
