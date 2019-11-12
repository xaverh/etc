#!/usr/bin/zsh

localectl set-keymap de
lsblk
ping google.com
ls /sys/firmware/efi/efivars

# efibootmgr
blkdiscard /dev/sdX
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda
mkfs.fat -F32 /dev/sda1

cryptsetup -y --use-random -v --type luks2 luksFormat /dev/sdX2
# SSD?
cryptsetup -y --use-random -v --type luks2 --align-payload=8192 luksFormat /dev/sdX2

cryptsetup open /dev/sdX2 cryptroot
# SSD?
cryptsetup --allow-discards --persistent open /dev/sdX2 cryptroot

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

cp /etc/pacman.conf ~
vim ~/pacman.conf

vim /etc/pacman.d/mirrorlist

pacstrap -c -C /mnt/etc/pacman.conf /mnt base linux linux-firmware vim zsh tmux man-db man-pages btrfs-progs sudo unrar zip unzip exfat-utils git mupdf-gl ncdu openssh p7zip pulseaudio rmlint clipmenu gimp herbstluftwm rofi mpv nnn youtube-dl pamixer slock sxiv telegram-desktop ttf-ibm-plex xclip xorg-xinit nodejs lua stow strawberry discord firefox weechat alacritty noto-fonts-emoji chromium opus-tools go

pacstrap -c -C /mnt/etc/pacman.conf /mnt cryptsetup intel-ucode amd-ucode broadcom-wl-dkms iw iwd xf86-video-intel bluez bluez-utils numlockx libva-intel-driver libdvdcss pulseaudio-bluetooth steam light rawtherapee gstreamer-vaapi abcde

# as-deps: linux-headers alacritty-terminfo gst-plugins-{bad,ugly} gst-libav x11-ssh-askpass pulseaudio-alsa clipnotify xorg-xsetroot npm
# as-explicit: less curl
# disect: xorg xorg-apps

sudo pacman -Rddns adobe-source-code-pro-fonts cantarell-fonts adwaita-icon-theme gnu-free-fonts xorg-fonts-100dpi xorg-fonts-75dpi gsfonts dmenu xorg-xfd xorg-xwud xorg-xvinfo lvm2 xorg-xpr xorg-xwd

# Essentials
# usr-local-lib-systemd-system-slock\x40.service
# etc-systemd-network-05\x2dwired.network
# etc-systemd-resolved.conf.d-10\x2dDNSSEC.conf
# etc-systemd-resolved.conf.d-20\x2d1.1.1.1.conf
# etc-X11-xorg.conf.d-20\x2ddontzap.conf
# boot-loader-loader.conf
# boot-loader-entries-arch.conf
echo "en_US.UTF-8 UTF-8" >> /mnt/etc/locale.gen

# As needed
# etc-systemd-network-10\x2dwireless.network
# usr-local-lib-systemd-user-ssh\x2dagent.service
# etc-X11-xorg.conf.d-15\x2dintel.conf
# etc-X11-xorg.conf.d-30\x2dinput.conf

/etc/mkinitcpio.conf
# HOOKS="base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck"
# sd-encrypt only needed if hd is encrypted

echo pts/0  >> /mnt/securetty

systemd-nspawn -D /mnt passwd root
systemd-nspawn -D /mnt chsh -s /usr/bin/zsh


systemd-nspawn -D /mnt useradd -m -N -g users -s /usr/bin/zsh xha
systemd-nspawn -D /mnt passwd xha
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
systemctl enable slock@xha.service

systemctl edit getty@tty1
# [Service]
# ExecStart=
# ExecStart=-/sbin/agetty --autologin xha --noclear %I $TERM

bootctl install

mkinitcpio -p linux

exit
reboot

ln -sf /run/systemd/resolve/stub-resolv.conf /mnt/etc/resolv.conf

# As user
xdg-mime default mupdf-gl.desktop application/pdf
xdg-mime default mupdf-gl.desktop application/vnd.comicbook-rar
xdg-mime default mupdf-gl.desktop application/vnd.comicbook+zip
xdg-mime default mupdf-gl.desktop application/epub+zip
xdg-mime default mupdf-gl.desktop application/x-cb7
xdg-mime default sxiv.desktop image/jpeg
xdg-mime default sxiv.desktop image/png
xdg-mime default sxiv.desktop image/gif
xdg-mime default sxiv.desktop image/tiff

npm -g i @vue/cli generator-code gulp-cli sass vsce yo

code \
--install-extension bierner.markdown-checkbox \
--install-extension bierner.markdown-footnotes \
--install-extension bierner.markdown-mermaid \
--install-extension dbaeumer.vscode-eslint \
--install-extension eg2.vscode-npm-script \
--install-extension esbenp.prettier-vscode \
--install-extension James-Yu.latex-workshop \
--install-extension ms-vscode.cpptools \
--install-extension ms-vscode.Go \
--install-extension ms-vscode.vscode-typescript-tslint-plugin \
--install-extension msjsdiag.debugger-for-chrome \
--install-extension nhoizey.gremlins \
--install-extension octref.vetur \
--install-extension pflannery.vscode-versionlens \
--install-extension sdras.night-owl \
--install-extension sdras.vue-vscode-snippets \
--install-extension trixnz.vscode-lua \
--install-extension twxs.cmake \
--install-extension VisualStudioExptTeam.vscodeintellicode \
--install-extension wmaurer.change-case \
--install-extension xaver.clang-format \
--install-extension xaver.theme-qillqaq \
--install-extension xaver.theme-ysgrifennwr

systemctl --user enable --now ssh-agent.service
systemctl --user enable --now clipmenud.service
