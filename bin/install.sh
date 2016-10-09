loadkeys de
lsblk
ping google.com
timedatectl set-ntp true

# EFI
# don't forget the EFI partition
# https://wiki.archlinux.org/index.php/EFI_System_Partition
cgdisk /dev/sdX

# BIOS
# check which filesystems syslinux can handle
cfdisk /dev/sdX

# make filesystems
# mount everything accordingly below /mnt

vi /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel

genfstab -U /mnt >> /mnt/etc/fstab
vi /mnt/etc/fstab
# add discard for ssds
arch-chroot /mnt

mkinitcpio -p linux

passwd

# Intel CPU
pacman -S intel-ucode

# BIOS
pacman -S syslinux
syslinux-install_update -i -m -a
vi /boot/syslinux/syslinux.cfg

# UEFI
bootctl --path=/boot install

cp /usr/share/systemd/bootctl/loader.conf /boot/loader.conf
cp /usr/share/systemd/bootctl/arch.conf /boot/loader/entries/
# get PARTUUID, put in arch.conf, and then tidy that mess!
ls -l /dev/disk/by-partuuid/ >> /boot/loader/entries/arch.conf
vi /boot/loader/entries/arch.conf

exit

# unmount all devices

reboot

# Time configuration
vi /etc/systemd/timesyncd.conf
timedatectl set-ntp true
timedatectl set-local-rtc false
timedatectl set-timezone Europe/Berlin

# Locale configuration
# This doesn't work yet, we are still missing LC_ALL and LANGUAGE
localectl set-locale LANG=en_US.UTF-8
localectl set-keymap de
localectl set-x11-keymap de

# network configuration
hostnamectl set-hostname myhostname
systemctl enable --now systemd-networkd

ln -s /dev/null /etc/udev/rules.d/80-net-setup-link.rules

echo "[Match]
Name=eth*
[Network]
DHCP=yes
[DHCP]
RouteMetric=10" > /etc/systemd/network/wired.network

echo "[Match]
Name=wlan*
[Network]
DHCP=yes
[DHCP]
RouteMetric=20" > /etc/systemd/network/wireless.network

ln -sf /run/systemd/resolve/resolv.conf /etc
/etc/systemd/resolved.conf
systemctl enable --now systemd-resolved

# Install optional packages
rankmirrors /etc/pacman.d/mirrorlist > /tmp/mirrorlist
vi /tmp/mirrorlist
mv /tmp/mirrorlist /etc/pacman.d/mirrorlist
vi /etc/pacman.conf
# enable multilib
pacman -Syu
# find video card
# Intel: pacman -S xf86-video-intel mesa-libgl lib32-mesa-libgl vulkan-intel
lspci | grep -e VGA -e 3D

pacman --needed -S  zsh rxvt-unicode vim clang lua noto-fonts{,-cjk,-emoji} screenfetch dunst scrot jsoncpp feh wget adobe-source-{code,sans,serif}-pro-fonts ttf-linux-libertine gimp zathura-{pdf-poppler,ps,djvu,cb} libstdc++5 llvm imagemagick unrar slock git abs mpd ncmpcpp unzip ttyload exfat-utils mpv youtube-dl numlockx npm nodejs mpc p7zip ranger xorg{,-apps,-fonts} firefox steam openmp texlive-most lib32-libpulse lib32-openal lib32-nss lib32-gtk2 lib32-gtk3 lib32-libcanberra lib32-gconf lib32-dbus-glib lib32-libnm-glib lib32-alsa-plugins pulseaudio pulseaudio-alsa pamixer alsa-utils bc aria2 lxappearance compton w3m

# Laptop?
pacman -Sq --noconfirm --needed xf86-input-synaptics acpi wpa_supplicant iw
ln -s /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
cp /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant.conf.bak
chmod a+r /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
# ctrl_interface=/var/run/wpa_supplicant
# eapol_version=1
# ap_scan=1
# fast_reauth=1
systemctl enable wpa_supplicant@wlan0
systemctl start wpa_supplicant@wlan0
# wpa_passphrase

# DVD?
pacman -Sq --noconfirm --needed libdvdcss

# users
useradd xha -m -G wheel -s /usr/bin/zsh
passwd xha
visudo

reboot
