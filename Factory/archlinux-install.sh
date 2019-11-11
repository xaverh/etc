blkdiscard /dev/sda

sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda

mkfs.fat -F32 /dev/sda1

cryptsetup --type luks2 --OPTIONS luksFormat /dev/sda2




mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt


mkdir -p /mnt/@/boot

mount /dev/sda1 /mnt/boot


pacstrap /mnt base linux linux-firmware iwd vim intel-ucode man-db man-pages broadcom-wl zsh sudo xorg slock rofi xclip lua nodejs npm zip stow git gst-plugins-{base,bad,good,ugly} gst-libav gstreamer-vaapi ncdu btrfs-progs

# xorg ^18

# ausgelassen ImageMagick steam strawberry gimp youtube-dl telegram-desktop discord weechat MozillaFirefox mpv sxiv flac pulseaudio{,-bluetooth}
# todo clipmenu

rm /mnt/etc/securetty

echo pts/0 >> /etc/securetty

echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen

localectl set-keymap de

# later
localectl set-x11-keymap de microsoftpro mac_nodeadkeys compose:rwin-altgr

# twice
cat > /mnt/etc/systemd/network/10-wireless.network <<"EOF"
[Match]
Name=wl*

[Network]
DHCP=true
IPv6AcceptRA=true
IPv6PrivacyExtensions=true

[DHCP]
UseDNS=false
UseNTP=false
RouteMetric=10

[IPv6AcceptRA]
UseDNS=false

EOF

