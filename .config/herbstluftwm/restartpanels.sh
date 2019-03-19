#!/usr/bin/env bash

herbstclient emit_hook quit_panel

for i in $(herbstclient list_monitors | cut -d':' -f1) ; do
    "$XDG_CONFIG_HOME/herbstluftwm/panel.sh" $i &
done
