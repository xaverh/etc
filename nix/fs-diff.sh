#!/usr/bin/env bash

set -euo pipefail

OLD_TRANSID=$(sudo btrfs subvolume find-new /mnt/@blank 9999999)
OLD_TRANSID=${OLD_TRANSID#transid marker was }

sudo btrfs subvolume find-new "/mnt/@" "$OLD_TRANSID" | sed '$d' | cut -f17- -d' ' | sort | uniq | while read path; do
	path="/$path"
	if [ -L "$path" ]; then
    		: # The path is a symbolic link, so is probably handled by NixOS already
  	elif [ -d "$path" ]; then
    		: # The path is a directory, ignore
	elif [ -f "$path" ]; then
		sudo ls -l "$path"
	else
		echo "Error: $path"
	fi
done
