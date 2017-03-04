loadkeys de
lsblk
ping google.com
timedatectl set-ntp true

# EFI
# don't forget the EFI partition
# https://wiki.archlinux.org/index.php/EFI_System_Partition
gdisk /dev/sdX

# BIOS
# check which filesystems syslinux can handle
fdisk /dev/sdX

# make filesystems
# mount everything accordingly below /mnt

vi /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel vim

genfstab -U /mnt >> /mnt/etc/fstab # settings for SSDs?
arch-chroot /mnt /bin/bash

passwd

# Intel CPU
pacman -S intel-ucode

# BIOS
pacman -S syslinux
syslinux-install_update -i -m -a
vi /boot/syslinux/syslinux.cfg

# UEFI
bootctl install

# /boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /intel-ucode.img
initrd  /initramfs-linux.img
options root=PARTUUID=f69907aa-e58a-4f56-8bc0-208e5f1ad73a rootfstype=xfs add_efi_memmap rw
# :r! blkid to get real PARTUUID

# /boot/loader/loader.conf
timeout 3
default arch

# replace "udev" with "systemd" in /etc/mkinitcpio.conf

mkinitcpio -p linux

exit

# unmount all devices

reboot

# Locale configuration
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
localectl list-locales
locale-gen
localectl set-locale LANG=en_US.UTF-8
localectl set-keymap de
localectl set-x11-keymap de

# Time configuration
vi /etc/systemd/timesyncd.conf # TODO
timedatectl set-ntp true
timedatectl set-local-rtc false # except on dual boot with Windows
timedatectl set-timezone Europe/Berlin

# network configuration
hostnamectl set-hostname myhostname

echo "[Match]
Name=en*
[Network]
DHCP=yes
[DHCP]
RouteMetric=10" > /etc/systemd/network/wired.network

echo "[Match]
Name=wl*
[Network]
DHCP=yes
[DHCP]
RouteMetric=20" > /etc/systemd/network/wireless.network

ln -sf /run/systemd/resolve/resolv.conf /etc
vim /etc/systemd/resolved.conf
systemctl enable --now systemd-resolved

# Install optional packages
rankmirrors /etc/pacman.d/mirrorlist > /tmp/mirrorlist
vi /tmp/mirrorlist
mv /tmp/mirrorlist /etc/pacman.d/mirrorlist
vi /etc/pacman.conf
# enable multilib
# enable Color
# add ILoveCandy
pacman -Syu
# find video card
lspci | grep -e VGA -e 3D
# Intel: pacman -S xf86-video-intel mesa-libgl lib32-mesa-libgl vulkan-intel
# NVidia: XXX

pacman --needed -S zsh rxvt-unicode clang lua noto-fonts{,-cjk,-emoji} \
 dunst scrot feh wget adobe-source-{code,sans,serif}-pro-fonts \
ttf-linux-libertine gimp zathura-{pdf-poppler,ps,djvu,cb} llvm imagemagick \
unrar slock git abs unzip ttyload exfat-utils mpv youtube-dl numlockx npm \
nodejs p7zip xorg{,-apps,-fonts,-xinit} gst-plugins-good gst-libav openmp \
texlive-most pulseaudio pulseaudio-alsa pamixer alsa-utils bc mac ttf-dejavu \
openssh xclip ssh-askpass go go-tools tmux

pacman --needed -S jsoncpp libstdc++5 mpd ncmpcpp mpc ranger firefox steam steam-native-runtime lib32-libpulse lib32-openal lib32-nss lib32-gtk2 lib32-gtk3 lib32-libcanberra lib32-gconf lib32-dbus-glib lib32-libnm-glib lib32-alsa-plugins aria2 lxappearance compton w3m cmatrix lolcat iperf3

# Laptop?
pacman --needed -S acpi wpa_supplicant iw

# XXX
# ln -s /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
vim /etc/wpa_supplicant/wpa_supplicant.conf
chmod a+r /etc/wpa_supplicant/wpa_supplicant.conf
# ctrl_interface=/var/run/wpa_supplicant
# eapol_version=1
# ap_scan=1
# fast_reauth=1
systemctl enable --now wpa_supplicant@wlan0 ## XXX
# wpa_passphrase

# DVD?
pacman --needed -S libdvdcss dvd+rw-tools

# SSD?
systemctl enable fstrim.timer

# users
useradd xha -m -G wheel -s /usr/bin/zsh
passwd xha
visudo
# Defaults insults

reboot
