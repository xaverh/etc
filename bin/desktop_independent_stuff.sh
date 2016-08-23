#!/bin/sh

## Detect and configure touchpad. See 'man synclient' for more info.
if egrep -iq 'touchpad' /proc/bus/input/devices; then
    synclient VertEdgeScroll=0 HorizEdgeScroll=0 TapButton1=1 VertScrollDelta=-15 HorizScrollDelta=-15 2>/dev/null
fi

# Gnome Keyring
eval "`gnome-keyring-daemon -d -s -c pkcs11,ssh,gpg,secrets`"
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
export GPG_AGENT_INFO

# start rxvt-unicode daemon
urxvtd &