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
Name=enp1s0

[Network]
DHCP=ipv4

[DHCP]
RouteMetric=10

/etc/systemd/network/wireless.network
[Match]
Name=wlp2s0

[Network]
DHCP=ipv4

[DHCP]
RouteMetric=20

systemctl start systemd-networkd.service
systemctl enable systemd-networkd.service

vi /etc/resolv.conf

# Install optional packages
rankmirrors /etc/pacman.d/mirrorlist > /tmp/mirrorlist
vi /tmp/mirrorlist
mv /tmp/mirrorlist /etc/pacman.d/mirrorlist
vi /etc/pacman.conf
pacman -Syu
pacman --needed -S  zsh rxvt-unicode vim clang lua xorg-xkill noto-fonts noto-fonts-cjk noto-fonts-emoji screenfetch jdk8-openjdk dunst pkgfile scrot jsoncpp feh xorg-xfontsel wget adobe-source-code-pro-fonts adobe-source-serif-pro-fonts adobe-source-sans-pro-fonts dmenu ttf-linux-libertine gimp zathura-pdf-poppler zathura-ps zathura-djvu zathura-cb libstdc++5 llvm imagemagick unrar slock xautolock git abs mpd ncmpcpp unzip ttyload exfat-utils mpv youtube-dl numlockx npm nodejs mpc p7zip zsh-syntax-highlighting ranger xorg-server xorg-server-utils xorg-xinit

# find video card
lspci | grep -e VGA -e 3D

# users
useradd xha -m -g users -G wheel,storage,power,network,video,audio,lp -s /usr/bin/zsh
visudo /etc/sudoers