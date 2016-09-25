loadkeys de
cat /proc/partitions
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

ln -s /usr/share/zoneinfo/Europe/Berlin /etc/localtime
hwclock --systohc --utc

vi /etc/locale.gen
#uncomment preferred locale

locale-gen

echo LANG=en_US.UTF-8 > /etc/locale.conf
echo KEYMAP=de > /etc/vconsole.conf

mkinitcpio -p linux

passwd

# BIOS
pacman -S syslinux
syslinux-install_update -i -m -a
vi /boot/syslinux/syslinux.cfg

# UEFI
cd /
bootctl --path=boot install

cp /usr/share/systemd/bootctl/loader.conf /boot/loader.conf
cp /usr/share/systemd/bootctl/arch.conf /boot/loader/entries/
# get PARTUUID, put in arch.conf, and then tidy that mess!
ls -l /dev/disk/by-partuuid/ >> /boot/loader/entries/arch.conf
vi /boot/loader/entries/arch.conf

# Intel CPU
pacman -S intel-ucode

exit

# unmount all devices

reboot


# network configuration
hostnamectl set-hostname myhostname
ln -s /dev/null /etc/udev/rules.d/80-net-setup-link.rules

/etc/systemd/network/wired.network
[Match]
Name=eth*

[Network]
DHCP=yes

[DHCP]
RouteMetric=10

/etc/systemd/network/wireless.network
[Match]
Name=wlan*

[Network]
DHCP=yes

[DHCP]
RouteMetric=20

systemctl start systemd-networkd.service
systemctl enable systemd-networkd.service

/etc/resolv.conf
nameserver 8.8.8.8
nameserver 8.8.4.4
nameserver 2001:4860:4860::8888
nameserver 2001:4860:4860::8844

# TODO: add resolvd systemd configuration

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

pacman --needed -S  zsh rxvt-unicode vim clang lua noto-fonts{,-cjk,-emoji} screenfetch jdk8-openjdk dunst pkgfile scrot jsoncpp feh wget adobe-source-{code,sans,serif}-pro-fonts dmenu ttf-linux-libertine gimp zathura-{pdf-poppler,ps,djvu,cb} libstdc++5 llvm imagemagick unrar slock git abs mpd ncmpcpp unzip ttyload exfat-utils mpv youtube-dl numlockx npm nodejs mpc p7zip zsh-syntax-highlighting ranger xorg-{server,server-utils,xinit,apps,xfontsel} firefox steam openmp texlive-most lib32-libpulse lib32-openal lib32-nss lib32-gtk2 lib32-gtk3 lib32-libcanberra lib32-gconf lib32-dbus-glib lib32-libnm-glib lib32-alsa-plugins pulseaudio pulseaudio-alsa pamixer alsa-utils bc

# Laptop?
sudo pacman -Sq --noconfirm --needed xf86-input-synaptics acpi wpa_supplicant iw
sudo ln -s /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
sudo cp /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant.conf.bak
sudo chmod a+r /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
sudo systemctl enable wpa_supplicant@wlan0
sudo systemctl start wpa_supplicant@wlan0

# DVD?
sudo pacman -Sq --noconfirm --needed libdvdcss

# users
useradd xha -m -g users -G wheel,storage,power,network,video,audio,lp -s /usr/bin/zsh
passwd xha
visudo

reboot
