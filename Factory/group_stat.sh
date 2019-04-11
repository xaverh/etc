#!/bin/bash

grpcur="<span foreground=\"blue\"> # </span>"
grpshown="<span foreground=\"blue\"> - </span>"
grpavail="<span foreground=\"blue\"> = </span>"

cur=`xprop -root _NET_CURRENT_DESKTOP | awk '{print $3}'`
#	tot=`xprop -root _NET_NUMBER_OF_DESKTOPS | awk '{print $3}'`
tot=10

for wid in `xprop -root -spy | sed '/_LIST(WINDOW)/!d;s/.*# //;s/,//g'`; do
	if grep -q 'IsViewable' <<< $(xwininfo -id $wid); then
		grp=`xprop -id $wid _NET_WM_DESKTOP | awk '{print $3}'`
		shown="$shown $grp"
	fi
done

for g in `seq 0 $((tot-1))`; do
	if test $g -eq $cur; then
		line="${line}<span foreground=\"#e5e6e6\"> ${g} </span>"
	elif grep -q $g <<< "$shown"; then
		line="${line}<span foreground=\"#969696\"> ${g} </span>"
	else
		line="${line}<span foreground=\"#515151\"> ${g} </span>"
	fi
done

echo $line
