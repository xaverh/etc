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
pacstrap /mnt base base-devel neovim bash-completion # intel-ucode amd-ucode
genfstab -U /mnt >> /mnt/etc/fstab
arch-chroot /mnt

passwd
bootctl install
vim /etc/mkinitcpio.conf
# HOOKS="base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck"
# sd-encrypt only needed if hd is encrypted

vim /boot/loader/loader.conf
# timeout 3
# default arch

vim /boot/loader/entries/arch.conf

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
pacman -Syu
# find video card
lspci | grep -e VGA -e 3D
# Intel: pacman -S xf86-video-intel mesa-libgl lib32-mesa-libgl vulkan-intel libva-intel-driver lib32-libva-intel-driver libvdpau-va-gl
# NVidia: XXX
# Laptop?
pacman --needed -S acpi iw iwd crda bluez bluez-utils

# Common:
pacman -S --needed unrar git unzip exfat-utils rtmpdump p7zip lzip pulseaudio pulseaudio-alsa alsa-utils mac stow ncdu speech-dispatcher openvpn tmux neofetch pacman-contrib udisks2 udevil openssh asp bind-tools wget

# optional dependencies
pacman -S --needed --asdeps python-neovim

# desktop:
pacman -S --needed dunst feh zathura-pdf-poppler zathura-ps zathura-djvu zathura-cb slock mpv numlockx xorg xorg-apps xorg-xinit xorg-server xorg-fonts xorg-fontsel xorg-xlsfonts xorg-xfd pamixer bc xclip x11-ssh-askpass dmenu playerctl polkit-gnome newsboat sxiv imagemagick autocutsel notion telegram-desktop firefox-developer-edition adobe-source-code-pro-fonts adobe-source-sans-pro-fonts adobe-source-serif-pro-fonts adobe-source-han-sans-otc-fonts adobe-source-han-serif-otc-fonts ttf-linux-libertine ttf-ibm-plex wqy-microhei pavucontrol ponymix clipmenu youtube-dl weechat rtorrent

# optional dependencies
pacman -S --asdeps python-pycryptodome gst-plugins-good gst-libav hunspell-en_US ttf-opensans

# software development:
pacman -S --needed valgrind gdb clang llvm cmake openmp nodejs npm go go-tools lua tokei texlive-most biber pandoc

# optional depedencies:
pacman -S --asdeps openmp

# applications:
pacman -S --needed steam rawtherapee gimp

# optional dependencies
pacman -S --asdeps steam-native-runtime

# optional packages
pacman -S --needed mpd ncmpcpp mpc ranger aria2 cmatrix lolcat iperf3 darktable libopenraw ttyload

# Uncomment the right regulatory domain in /etc/conf.d/wireless-regdom.

# diable power save mode for wifi
# source: https://bbs.archlinux.org/viewtopic.php?id=196375
iw dev wlp2s0 set power_save off

systemctl enable --now iwd

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
