#!/usr/bin/zsh

mkdir -p /tmp/mansplain/`date +"%G-%V"`
manpage=`apropos . | dmenu -l 30  | awk '{gsub(/[()]/,""); print $2" "$1}'`
filename="/tmp/mansplain/`date +"%G-%V"`/${^manpage}.pdf"
[[ -f "$filename" ]] || man -Tpdf ${=manpage} > "$filename"
mupdf "$filename"