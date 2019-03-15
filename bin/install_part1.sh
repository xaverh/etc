#!/usr/bin/env bash

localectl set-keymap de
lsblk
ping google.com
ls /sys/firmware/efi/efivars
timedatectl set-ntp true

gdisk /dev/sdX
partprobe
cryptsetup -y --use-random -v luksFormat /dev/sdX2
cryptsetup open /dev/sdX2 cryptroot

mkfs.xfs -L Archlinux /dev/mapper/cryptroot
mkfs.vfat /dev/sdX1
mkswap /dev/sdX3
mount /dev/mapper/cryptroot /mnt
mkdir /mnt/boot
mount /dev/sdX1 /mnt/boot
swapon /sev/sdX3

vi /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel neovim bash-completion tmux # intel-ucode amd-ucode
genfstab -U /mnt >> /mnt/etc/fstab
arch-chroot /mnt

passwd
bootctl install
nvim /etc/mkinitcpio.conf
# HOOKS="base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck"
# sd-encrypt only needed if hd is encrypted

nvim /boot/loader/loader.conf
# timeout 3
# default arch

nvim /boot/loader/entries/arch.conf

# generate locale
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen

vim /etc/vconsole.conf
# KEYMAP=de

mkinitcpio -p linux

useradd xha -m -G wheel
passwd xha
visudo
# Defaults insults
# Defaults env_keep += "EDITOR"

exit
reboot
