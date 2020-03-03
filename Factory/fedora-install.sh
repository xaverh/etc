#!/usr/bin/zsh

localectl set-keymap de
lsblk
ping google.com
ls /sys/firmware/efi/efivars

sudo -s

# efibootmgr
blkdiscard /dev/sda

# enc
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda

# not enc
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8304 /dev/sda

mkfs.fat -F32 /dev/sda1

cryptsetup -y --use-random -v --type luks2 luksFormat /dev/sda2
# SSD?
cryptsetup -y --use-random -v --type luks2 --align-payload=8192 luksFormat /dev/sda2

cryptsetup open /dev/sda2 cryptroot
# SSD?
cryptsetup --allow-discards --persistent open /dev/sda2 cryptroot

cryptsetup luksUUID /dev/sda2
# 04f7a64c-e13f-4a09-a2bb-afbfc3c45390

mkfs.btrfs /dev/mapper/cryptroot

# btrfs-progs v5.2.1
# See http://btrfs.wiki.kernel.org for more information.
#
# Detected a SSD, turning off metadata duplication.  Mkfs with -m dup if you want to force metadata duplication.
# Label:              (null)
# UUID:               23ccb92a-f945-4ef6-aecc-e32b46840ee1
# Node size:          16384
# Sector size:        4096
# Filesystem size:    111.58GiB
# Block group profiles:
#   Data:             single            8.00MiB
#   Metadata:         single            8.00MiB
#   System:           single            4.00MiB
# SSD detected:       yes
# Incompat features:  extref, skinny-metadata
# Number of devices:  1
# Devices:
#    ID        SIZE  PATH
#     1   111.58GiB  /dev/mapper/cryptroot

mount -o compress-force=zstd:6,lazytime /dev/mapper/cryptroot /mnt

btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@/home
mkdir /mnt/@/var
chattr +C /mnt/@/var
mkdir /mnt/@/efi
btrfs subvolume set-default /mnt/@
umount /mnt
mount -o compress-force=zstd:6,lazytime /dev/mapper/cryptroot /mnt
mount /dev/sda1 /mnt/efi

dnf config-manager --enablerepo fedora-cisco-openh264
dnf install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
dnf install https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
dnf install rpmfusion-free-release-tainted rpmfusion-nonfree-release-tainted
rpm --import https://packages.microsoft.com/keys/microsoft.asc
echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo
rpm --import https://dl.google.com/linux/linux_signing_key.pub
echo -e "[google-chrome]\nname=google-chrome\nbaseurl=http://dl.google.com/linux/chrome/rpm/stable/x86_64\nenabled=1\ngpgcheck=1\ngpgkey=https://dl.google.com/linux/linux_signing_key.pub" > /etc/yum.repos.d/google-chrome.repo
dnf copr enable pschyska/alacritty
dnf copr enable jdoss/wireguard
dnf config-manager --add-repo https://download.opensuse.org/repositories/home:/xha/Fedora_$(rpm -E %fedora)/home:xha.repo
dnf check-update

# Add following options to /etc/dnf/dnf.conf
# exclude=NetworkManager plymouth* PackageKit-gstreamer-plugin abattis-cantarell-fonts fedora-bookmarks dhcp-client gnome-keyring mercurial subversion wireless-tools hddtemp
# xorg-x11-drv-ati xorg-x11-drv-nouveau xorg-x11-drv-intel

dnf install --installroot=/mnt --releasever=/ @core zsh glibc-langpack-en neovim btrfs-progs util-linux-user rpmfusion-free-release-tainted rpmfusion-nonfree-release-tainted sqlite dbus-tools
# iwd wireless-regdb broadcom-wl cryptsetup

systemd-firstboot --root=/mnt --locale=en_US.UTF-8 --keymap=us --hostname=airolo --setup-machine-id

cp ~/etc/Factory/etc-systemd-network-05\\x2dwired.network /mnt/etc/systemd/network/05-wired.network
mkdir /mnt/etc/systemd/resolved.conf.d
cp ~/etc/Factory/etc-systemd-resolved.conf.d-20\\x2d1.1.1.1.conf /mnt/etc/systemd/resolved.conf.d/20-1.1.1.1.conf
mkdir /mnt/etc/iwd
cp ~/etc/Factory/etc-iwd-main.conf /mnt/etc/iwd/main.conf
cp -nv /etc/yum.repos.d/* /mnt/etc/yum.repos.d/
cp /etc/dnf/dnf.conf /mnt/etc/dnf/dnf.conf

systemd-nspawn -D /mnt chsh -s /usr/bin/zsh
systemd-nspawn -D /mnt useradd -m -U -G wheel -s /usr/bin/zsh xha
setenforce 0
systemd-nspawn -D /mnt passwd xha
setenforce 1

systemd-nspawn -bD /mnt

sudo bootctl install
sudo dnf install @base-x @multimedia @firefox google-chrome-stable code gimp mpv youtube-dl ffmpeg telegram-desktop discord flameshot pavucontrol nnn rmlint unrar unzip exfat-utils git nodejs golang lua @c-development man-pages clipmenu clipnotify xclip sent slock alacritty google-noto-emoji-color-fonts dmz-cursor-themes unicode-emoji x11-ssh-askpass strawberry awesome zathura zathura-cb zathura-djvu zathura-pdf-mupdf zathura-ps playerctl mpv-mpris abduco dvtm rofi
# wireguard-dkms wireguard-tools

sudo dnf install iw libdvdcss bluez bluez-tools pulseaudio-module-bluetooth-freeworld steam rawtherapee libva-intel-driver abcde gstreamer1-vaapi
# libva-intel-hybrid-driver weechat

localectl set-x11-keymap us pc104 altgr-intl compose:menu,rupeesign:4
localectl set-x11-keymap de apple_laptop mac_nodeadkeys compose:rwin-altgr
timedatectl set-timezone Europe/Berlin
sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
systemctl enable --now systemd-resolved.service
systemctl enable --now systemd-timesyncd.service
systemctl enable iwd.service
systemctl enable fstrim.timer

sudo dnf install kernel

# kernel boot line
# root=PARTUUID=0c8461cd-db5c-4249-96b8-18451311aab0 rd.luks.crypttab=0 rw rd.lvm=0 rd.md=0 rootflags=defaults,lazytime,compress-force=zstd:6,ssd i915.fastboot=1
# root=UUID=23ccb92a-f945-4ef6-aecc-e32b46840ee1 rd.luks.uuid=04f7a64c-e13f-4a09-a2bb-afbfc3c45390 rd.luks.crypttab=0 rw rd.lvm=0 rd.md=0 rootflags=defaults,lazytime,compress-force=zstd:6,ssd i915.fastboot=1

sudo touch /.autorelabel

sudo dnf install kernel

exit
reboot

timedatectl set-ntp true

systemctl enable slock@xha.service
# Essentials
mkdir -p /mnt/usr/local/lib/systemd/system
cp ~/etc/Factory/usr-local-lib-systemd-system-slock\\x40.service /mnt/usr/local/lib/systemd/system/slock@.service
cp ~/etc/Factory/etc-polkit\x2d1-rules.d-49\x2dnopasswd_limited.rules /mnt/etc/polkit-1/rules.d/49-nopasswd_limited.rules
cp ~/etc/Factory/etc-systemd-system-getty\\x40tty1.service.d-override.conf /mnt/etc/systemd/system/getty@tty1.service.d/override.conf
cp ~/etc/Factory/etc-systemd-system-iwd.service.d-override.conf /mnt/etc/systemd/system/iwd.service.d/override.conf
cp ~/etc/Factory/etc-dracut.conf.d-local.conf /mnt/etc/dracut.conf.d/local.conf
# As needed
cp ~/etc/Factory/etc-udev-rules.d-90\\x2dbacklight.rules /mnt/etc/udev/rules.d/90-backlight.rules

echo 'w /sys/module/hid_apple/parameters/fnmode - - - - 2' | sudo tee /etc/tmpfiles.d/rev_fn_key.conf
echo 'fs.inotify.max_user_watches=524288' | sudo tee /etc/sysctl.d/95-max-user-watches.conf

# disable root account

# As user
xdg-mime default org.pwmt.zathura.desktop application/pdf application/vnd.comicbook-rar application/vnd.comicbook+zip application/epub+zip application/x-cb7
xdg-mime default sxiv.desktop image/jpeg image/png image/gif image/tiff image/webp image/x-xpmi
xdg-mime default nnn.desktop inode/directory
desktop-file-install --rebuild-mime-info-cache --dir="$XDG_DATA_HOME/applications" ~/.config/Factory/nnn.desktop && xdg-settings set default-url-scheme-handler file nnn.desktop

npm -g i @vue/cli generator-code gulp-cli sass vsce yo neovim

code --install-extension bierner.markdown-checkbox --install-extension bierner.markdown-footnotes --install-extension bierner.markdown-mermaid --install-extension dbaeumer.vscode-eslint --install-extension eg2.vscode-npm-script --install-extension esbenp.prettier-vscode --install-extension firefox-devtools.vscode-firefox-debug --install-extension James-Yu.latex-workshop --install-extension ms-python.python --install-extension ms-vscode.cpptools --install-extension ms-vscode.Go --install-extension ms-vscode.vscode-typescript-tslint-plugin --install-extension msjsdiag.debugger-for-chrome --install-extension nhoizey.gremlins --install-extension octref.vetur --install-extension pflannery.vscode-versionlens --install-extension sdras.night-owl --install-extension sdras.vue-vscode-snippets --install-extension trixnz.vscode-lua --install-extension twxs.cmake --install-extension VisualStudioExptTeam.vscodeintellicode --install-extension wmaurer.change-case --install-extension xaver.clang-format --install-extension xaver.theme-qillqaq --install-extension xaver.theme-ysgrifennwr

systemctl enable sshd.socket

# Add to /etc/pulse/default.pa
# load-module module-switch-on-connect