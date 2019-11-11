#!/usr/bin/env bash

localectl set-keymap de
lsblk
ping google.com
ls /sys/firmware/efi/efivars
timedatectl set-ntp true

blkdiscard /dev/sdX
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda
mkfs.fat -F32 /dev/sda1

cryptsetup -y --use-random -v --type luks2 luksFormat /dev/sdX2
# SSD?
cryptsetup -y --use-random -v --type luks2 --align-payload=8192 luksFormat /dev/sdX2

cryptsetup open /dev/sdX2 cryptroot
# SSD?
cryptsetup --allow-discards --persistent open /dev/sda2 cryptroot

mkfs.btrfs /dev/mapper/cryptroot
mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt

btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@/home
mkdir /mnt/@/var
chattr +C /mnt/@/var
mkdir /mnt/@/boot
btrfs subvolume set-default /mnt/@
cd /
umount /mnt
mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt
mount /dev/sda1 /mnt/boot

cp /etc/pacman.conf /mnt/etc/pacman.conf

vim /mnt/etc/pacman.conf

vim /etc/pacman.d/mirrorlist

pacstrap -c -C /mnt/etc/pacman.conf /mnt base linux linux-firmware vim zsh tmux man-db man-pages sudo less alsa-utils exfat-utils git mupdf-gl ncdu openssh p7zip x11-ssh-askpass pulseaudio{,-alsa} rmlint unrar unzip clipmenu gimp herbstluftwm rofi clipnotify mpv nnn youtube-dl pamixer slock sxiv telegram-desktop ttf-ibm-plex xclip xorg xorg-xinit xorg-xfd nodejs lua npm stow zip git gst-plugins-{base,bad,good,ugly} gst-libav gstreamer-vaapi btrfs-progs strawberry discord firefox weechat alacritty
pacstrap  -c -C /mnt/etc/pacman.conf /mnt cryptsetup intel-ucode amd-ucode broadcom-wl-dkms iw iwd xf86-video-intel bluez bluez-utils numlockx libva-intel-driver libdvdcss pulseaudio-bluetooth steam

# as-deps: linux-headers

# differences: pamixer slock ttf-ibm-plex xorg-xfd

echo pts/0  >> /mnt/securetty

systemd-nspawn -D /mnt passwd root

systemd-nspawn -D /mnt chsh -s /usr/bin/zsh

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

mkdir -p /mnt/etc/systemd/resolved.conf.d
cat > /mnt/etc/systemd/resolved.conf.d/10-DNSSEC.conf <<"EOF"
[Resolve]
DNSSEC=false

EOF

cat > /mnt/etc/systemd/resolved.conf.d/20-1.1.1.1.conf <<"EOF"
[Resolve]
DNS=1.1.1.1 2606:4700:4700::1111
FallbackDNS=1.0.0.1 2606:4700:4700::1001

EOF

ln -sf /run/systemd/resolve/stub-resolv.conf /mnt/etc/resolv.conf

vim /etc/mkinitcpio.conf
# HOOKS="base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck"
# sd-encrypt only needed if hd is encrypted

systemd-nspawn -D /mnt useradd -m -N -g users -s /usr/bin/zsh xha
systemd-nspawn -D /mnt passwd xha

echo "en_US.UTF-8 UTF-8" >> /mnt/etc/locale.gen

systemd-nspawn -D /mnt locale-gen

cp /mnt/etc/sudoers /mnt/etc/sudoers.d/sudoers

systemd-nspawn -D /mnt --setenv=EDITOR=vim visudo /etc/sudoers.d/sudoers


systemd-nspawn -bD /mnt

localectl set-locale LANG=en_US.UTF-8
localectl set-x11-keymap us pc104 altgr-intl "compose:menu"
localectl set-x11-keymap de apple_laptop mac_nodeadkeys compose:rwin-altgr
timedatectl set-ntp true
timedatectl set-timezone Europe/Berlin
hostnamectl set-hostname andermatt
systemctl enable --now systemd-resolved.service
systemctl enable --now systemd-networkd.service
systemctl enable --now systemd-timesyncd.service
systemctl enable --now iwd.service

bootctl install

vim /boot/loader/loader.conf

timeout 1
default arch

vim /boot/loader/entries/arch.conf

mkinitcpio -p linux

exit
reboot
