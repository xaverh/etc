localectl set-keymap de
lsblk
ping google.com
ls /sys/firmware/efi/efivars
timedatectl set-ntp true

gdisk /dev/sdX
cryptsetup -y --use-random -v luksFormat /dev/sdX2
cryptsetup open /dev/sdX2 cryptroot

mkfs.xfs -L Archlinux /dev/mapper/cryptroot
mkfs.vfat /dev/sdX1
mount /dev/mapper/cryptroot /mnt
mkdir /mnt/boot
mount /dev/sdX1 /mnt/boot

vi /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel vim bash-completion intel-ucode wpa_supplicant
genfstab -U /mnt >> /mnt/etc/fstab
arch-chroot /mnt

passwd
bootctl install
vim /etc/mkinitcpio.conf
# replace "udev" with "systemd" in /etc/mkinitcpio.conf
# encrypted:
# HOOKS="systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck"

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
# options rw rd.luks.uuid=`UUID of /dev/mapper/cryptroot` root=UUID=`UUID of /dev/sdX2`

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
timedatectl set-local-rtc false
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

pacman --needed -S clang lua dunst scrot feh zathura-{pdf-poppler,ps,djvu,cb} llvm imagemagick pavucontrol unrar slock git unzip exfat-utils mpv youtube-dl rtmpdump numlockx npm nodejs p7zip xorg{,-apps,-xinit,-server} gst-plugins-good gst-libav openmp texlive-most biber pulseaudio pulseaudio-alsa pamixer alsa-utils bc mac openssh xclip x11-ssh-askpass go go-tools stow dmenu ncdu playerctl firefox speech-dispatcher openvpn adobe-source-{code,sans,serif}-pro-fonts adobe-source-han-{sans,serif}-otc-fonts avahi mc htop udisks2 tmux polkit-gnome keybase kbfs neofetch pacman-contrib

# optional packages
pacman --needed -S jsoncpp mpd ncmpcpp mpc ranger steam steam-native-runtime lib32-gtk3steam lib32-gtk3 aria2 cmatrix lolcat iperf3 darktable ttf-linux-libertine gimp libopenraw ttyload pcmanfm libstdc++5 xorg-fonts btrfs-progs

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
# Defaults env_keep += "EDITOR"

systemctl edit getty@tty1
# [Service]
# ExecStart=
# ExecStart=-/usr/bin/agetty --autologin xha --noclear %I $TERM

reboot
