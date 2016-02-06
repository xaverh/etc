#!/bin/bash

set -eu

rname=gnome-shell-theme.gresource
resource="/usr/share/gnome-shell/$rname"

ext="$(date +%s)$$"
tmpdir="./fix_$ext"
mkdir "$tmpdir"
trap "rm -f $tmpdir/* ; rmdir $tmpdir" 0

manifest="$rname.xml"
cat > "$tmpdir/$manifest" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<gresources>
<gresource prefix="/org/gnome/shell/theme">
EOF

for file in $(gresource list "$resource"); do
    base=$(basename "$file")
    out="$tmpdir/$base"
    gresource extract "$resource" "$file" > "$out"
    echo "<file>$base</file>" >> "$tmpdir/$manifest"
done

cat >> "$tmpdir/$manifest" <<EOF
</gresource>
</gresources>
EOF

(
    cd "$tmpdir"
    perl -i -p -e 's/font-family:.*;/font-family: "System Font", Cantarell, Sans-Serif;/' gnome-shell.css
    glib-compile-resources "$manifest"
)

sudo cp "$resource" "$resource.dist.$ext"
sudo cp "$tmpdir/$rname" "$resource"

