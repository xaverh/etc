localectl set-keymap de
lsblk
ping google.com
ls /sys/firmware/efi/efivars
timedatectl set-ntp true

gdisk /dev/sdX
cryptsetup -y --use-random -v luksFormat /dev/sdX2
cryptsetup open /dev/sdX2 cryptroot

# XFS, etc.
mkfs.xfs /dev/mapper/cryptroot
mkfs.vat /dev/sdX1
mount /dev/mapper/cryptroot /mnt
mkdir /mnt/boot
mount /dev/sdX1 /mnt/boot

# btrfs
mkfs.btrfs -L Archlinux /dev/mapper/cryptroot
mkfs.vat /dev/sdX1
mount -o compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt
cd /mnt
btrfs subvolume create @
btrfs subvolume create @home
btrfs subvolume create @usr-local
btrfs subvolume create @var-tmp
# btrfs subvolume create @var-lib-machines
btrfs subvolume create @var-cache
btrfs subvolume create @var-log
btrfs subvolume create @var-lib-pacman
mkdir -p @var-cache/pacman/pkg
btrfs subvolume create @var-cache-pacman-pkg
cd /
umount /mnt
mount -o subvol=@,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt
mkdir -p /mnt/home
mkdir -p /mnt/snapshots
mkdir -p /mnt/tmp
mkdir -p /mnt/usr/local
mkdir -p /mnt/var/tmp
mkdir -p /mnt/var/lib/machines
mkdir -p /mnt/var/cache
mkdir -p /mnt/var/spool
mkdir -p /mnt/var/log
mkdir -p /mnt/var/cache
mkdir -p /mnt/boot
mount -o subvol=@,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt
mount -o subvol=@home,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/home
mount -o subvol=@snapshots,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/.snapshots
mount -o subvol=@tmp,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/tmp
mount -o subvol=@usr-local,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/usr/local
mount -o subvol=@var-tmp,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/tmp
mount -o subvol=@var-lib-machines,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/lib/machines
mount -o subvol=@var-cache,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/cache
mount -o subvol=@var-log,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/log
mount -o subvol=@var-spool,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/spool
mount -o subvol=@var-lib-pacman,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/lib/pacman
mount -o subvol=@var-cache-pacman-pkg,compress=lzo,space_cache,noatime,ssd /dev/mapper/cryptroot /mnt/var/cache/pacman/pkg

vi /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel vim bash-completion intel-ucode btrfs-progs wpa_supplicant
genfstab -U /mnt >> /mnt/etc/fstab
arch-chroot /mnt

passwd
bootctl install
vim /etc/mkinitcpio.conf
# replace "udev" with "systemd" in /etc/mkinitcpio.conf
# encrypted:
# HOOKS="base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck"
# btrfs does not need fsck

vim /boot/loader/loader.conf
# timeout 3
# default arch

vim /boot/loader/entries/arch.conf
# title   Arch Linux
# linux   /vmlinuz-linux
# initrd  /intel-ucode.img
# initrd  /initramfs-linux.img
# options rw root=PARTUUID=f699...
# # :r! lsblk -n -o PARTUUID /dev/sdb1 to get real PARTUUID
# options rw rd.luks.uuid=`UUID of /dev/mapper/cryptroot` luks.options=timeout=0s root=UUID=`UUID of /dev/sdX2` rootflags=subvol=@

# generate locale
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen

vim /etc/vconsole.conf
# KEYMAP=de

mkinitcpio -p linux
exit
reboot

localectl set-locale LANG=en_US.UTF-8
localectl set-keymap de

vim /etc/systemd/timesyncd.conf
timedatectl set-ntp true
timedatectl set-local-rtc false # except on dual boot with Windows
timedatectl set-timezone Europe/Berlin

# network configuration
hostnamectl set-hostname myhostname
vim /etc/systemd/network/wired.network

vim /etc/systemd/network/wireless.network

ln -sf /run/systemd/resolve/resolv.conf /etc
vim /etc/systemd/resolved.conf
vim /etc/nsswitch.conf
# hosts: files mymachines resolve myhostname

systemctl enable --now systemd-resolved
systemctl enable --now systemd-networkd

vim /etc/pacman.d/mirrorlist
vim /etc/pacman.conf
# enable multilib
# enable Color VerbosePkgLists
# add ILoveCandy
pacman -Syu
# find video card
lspci | grep -e VGA -e 3D
# Intel: pacman -S xf86-video-intel mesa-libgl lib32-mesa-libgl vulkan-intel
# NVidia: XXX

pacman --needed -S clang lua dunst scrot feh zathura-{pdf-poppler,ps,djvu,cb} llvm imagemagick pavucontrol unrar slock git unzip exfat-utils mpv youtube-dl rtmpdump numlockx npm nodejs p7zip xorg{,-apps,-xinit} gst-plugins-good gst-libav openmp texlive-most pulseaudio pulseaudio-alsa pamixer alsa-utils bc mac openssh xclip x11-ssh-askpass go go-tools stow dmenu ncdu playerctl firefox openvpn adobe-source-{code,sans,serif}-pro-fonts adobe-source-han-{sans,serif}-otc-fonts noto-fonts-emoji avahi mc abduco

# optional packages
pacman --needed -S jsoncpp mpd ncmpcpp mpc ranger steam steam-native-runtime lib32-gtk3 aria2 w3m cmatrix lolcat iperf3 darktable ttf-linux-libertine gimp libopenraw ttyload pcmanfm libstdc++5 xorg-fonts vifm

# Laptop?
pacman --needed -S acpi iw wireless_tools # is wireless_tools deprecated?

vim /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
# ctrl_interface=/var/run/wpa_supplicant
# eapol_version=1
# ap_scan=1
# fast_reauth=1

systemctl enable --now wpa_supplicant@wlan0

systemctl enable --now avahi-daemon

# DVD?
pacman --needed -S libdvdcss dvd+rw-tools

# SSD?
systemctl enable fstrim.timer

useradd xha -m -G wheel
passwd xha
visudo
# Defaults insults

systemctl edit getty@tty1
# [Service]
# ExecStart=
# ExecStart=-/usr/bin/agetty --autologin xha --noclear %I $TERM

reboot
