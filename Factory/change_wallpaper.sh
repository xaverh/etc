#!/bin/sh

xsetroot -bitmap $HOME/etc/backdrops/`ls $HOME/etc/backdrops | shuf -n 1 | tr -d '\n' | tee -a /tmp/wallpaper` `printf -- " -fg #%06x -bg #%06x\n" $(shuf -i0-16777215 -n2) | tee -a /tmp/wallpaper`