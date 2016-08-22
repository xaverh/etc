#!/bin/sh

## Detect and configure touchpad. See 'man synclient' for more info.
if egrep -iq 'touchpad' /proc/bus/input/devices; then
    synclient VertEdgeScroll=0 HorizEdgeScroll=0 TapButton1=1 VertScrollDelta=-15 HorizScrollDelta=-15 2>/dev/null
fi

## GNOME PolicyKit and Keyring
eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg) &

# start rxvt-unicode daemon
urxvtd &