#!/usr/bin/env bash

nvim /etc/pacman.d/mirrorlist
nvim /etc/pacman.conf
# enable multilib
# enable Color VerbosePkgLists

sudo cp $HOME/etc/sysctl.d/00-local-userns.conf /etc/sysctl.d/00-local-userns.conf

pacman -Syu

# find video card `lspci | grep -e VGA -e 3D`
# Intel graphics:
pacman -S lib32-libva-intel-driver  \
lib32-mesa-libgl \
libva-intel-driver \
libvdpau-va-gl \
mesa-libgl \
vulkan-intel \
xf86-video-intel

# Wifi/Bluetooth?
pacman -S acpi \
bluez \
bluez-utils \
iw \
iwd
pacman -S --asdeps --needed crda

# Common:
# ? ## ?
pacman -S alsa-utils \
# not on openSUSE ## not on Fedora
asp \
# ? ## not on Fedora
bind-tools \
exfat-utils \
git \
## ?
lzip \
# not on suse ## ?
mac \
ncdu \
openssh \
openvpn \
## ?
p7zip \
# not on openSUSE ## not on Fedora
pacman-contrib \
pulseaudio \
# ?
pulseaudio-alsa \
# not on SuSE
rmlint \
# not on SuSE
rtmpdump \
rsync \
# ?
speech-dispatcher \
# ?
udisks2 \
unrar \
unzip \
# ? anything cURL cannot do?
wget
pacman -S --asdeps --needed python-neovim \
ntfs-3g \
# ?
x11-ssh-askpass

# desktop:
pacman -S adobe-source-code-pro-fonts \
adobe-source-han-sans-otc-fonts \
adobe-source-han-serif-otc-fonts \
adobe-source-sans-pro-fonts \
adobe-source-serif-pro-fonts \
autocutsel \
# not on suse
clipmenu \
dmenu \
dunst \
firefox-developer-edition \
gimp \
herbstluftwm \
imagemagick \
mpv \
newsboat \
nnn \
# ?
numlockx \
pamixer \
pavucontrol \
playerctl \
rtorrent \
# ?
slock \
sxiv \
telegram-desktop \
# not on SuSE
ttf-ibm-plex \
# ?
ttf-linux-libertine \
weechat \
# ?
wqy-microhei \
xclip \
xorg \
# not on SuSE
xorg-apps \
xorg-fonts \
xorg-server \
# not on SuSE
xorg-xfd \
# not on SuSE
xorg-xfontsel \
# not on SuSE
xorg-xinit \
# not on SuSE
xorg-xlsfonts \
youtube-dl \
zathura-cb \
zathura-djvu \
zathura-pdf-poppler \
zathura-ps
# not on SuSE
pacman -S --asdeps --needed clipnotify \
dzen2 \
# ?
gst-libav \
# ?
gst-plugins-good \
# ?
hunspell-en_US \
# ?
libheif \
# ?
libraw \
# ?
poppler-data \
# ?
python-pycryptodome \
# ?
ttf-opensans

# software development:
pacman -S biber \
clang \
cmake \
# ?
gdb \
go \
# ?
go-tools \
llvm \
# ?
lua \
nodejs \
pandoc \
texlive-most \
# ?
tokei \
# ?
valgrind
pacman -S --asdeps --needed npm \
# ?
openmp \
# ?
pandoc-crossref

# other:
pacman -S mpc \
mpd \
ncmpcpp \
neofetch \
# not on SuSE
rawtherapee \
steam
pacman -S --asdeps --needed gst-plugins-bad \
gst-plugins-base \
lib32-libpulse \
steam-native-runtime

# pacman -S aria2 cmatrix darktable iperf3 libopenraw lolcat ttyload

# DVD?
pacman -S dvd+rw-tools
# not on SuSE
pacman -S  --asdeps --needed libdvdcss

localectl set-locale LANG=en_US.UTF-8
localectl set-keymap de

nvim /etc/systemd/timesyncd.conf
timedatectl set-ntp true
timedatectl set-local-rtc false
timedatectl set-timezone Europe/Berlin

# network configuration
hostnamectl set-hostname myhostname
sudo cp $HOME/etc/systemd/network/wired.network /etc/systemd/network/wired.network
ln -sf /run/systemd/resolve/resolv.conf /etc
sudo cp $HOME/etc/systemd/resolved.conf /etc/systemd/resolved.conf
sudo cp $HOME/etc/nsswitch.conf /etc/nsswitch.conf

systemctl enable --now systemd-resolved
systemctl enable --now systemd-networkd
systemctl enable --now avahi-daemon

# wireless?
sudo cp $HOME/etc/systemd/network/wireless.network /etc/systemd/network/wireless.network
# diable power save mode for wifi
# source: https://bbs.archlinux.org/viewtopic.php?id=196375
iw dev wlp2s0 set power_save off
# Uncomment the right regulatory domain in /etc/conf.d/wireless-regdom.
systemctl enable --now iwd

sudo cp -r $HOME/etc/openvpn/client /etc/openvpn

# SSD?
systemctl enable fstrim.timer


systemctl edit getty@tty1
# [Service]
# ExecStart=
# ExecStart=-/usr/bin/agetty --autologin xha --noclear %I $TERM

timedatectl set-ntp 1

systemctl enable --user slock
systemctl enable --user ssh-agent

systemctl enable --user clipmenud

curl -L "https://go.microsoft.com/fwlink/?LinkID=620884" | tar xz -C ~/lib

# sudo pacman -U https://github.com/xaverh/Iosevka/releases/download/v2.2.0-1-arch/ttf-iosifovich-r1189.30b3ffe9-1-any.pkg.tar.xz https://github.com/xaverh/Iosevka/releases/download/v2.2.0-1-arch/ttf-iosifovich-term-r1189.30b3ffe9-1-any.pkg.tar.xz

sudo cp $HOME/etc/X11/Xresources /etc/X11/Xresources
sudo cp $HOME/etc/X11/xorg.conf.d/20-slock.conf /etc/X11/xorg.conf.d/20-slock.conf
sudo cp $HOME/etc/X11/xorg.conf.d/30-touchpad.conf /etc/X11/xorg.conf.d/30-touchpad.conf
sudo cp $HOME/etc/X11/xorg.conf.d/31-pointer.conf /etc/X11/xorg.conf.d/31-pointer.conf

code \
--install-extension bierner.markdown-checkbox \
--install-extension bierner.markdown-emoji \
--install-extension bierner.markdown-footnotes \
--install-extension bierner.markdown-mermaid \
--install-extension chenxsan.vscode-standardjs \
--install-extension eg2.vscode-npm-script \
--install-extension hoovercj.vscode-settings-cycler \
--install-extension idleberg.emoji-code \
--install-extension James-Yu.latex-workshop \
--install-extension ms-vscode.cpptools \
--install-extension ms-vscode.Go \
--install-extension ms-vscode.vscode-typescript-tslint-plugin \
--install-extension msjsdiag.debugger-for-chrome \
--install-extension nhoizey.gremlins \
--install-extension octref.vetur  \
--install-extension pflannery.vscode-versionlens \
--install-extension sdras.vue-vscode-snippets \
--install-extension trixnz.vscode-lua \
--install-extension twxs.cmake \
--install-extension wmaurer.change-case \
--install-extension xaver.clang-format \
--install-extension xaver.theme-qillqaq \
--install-extension xaver.theme-ysgrifennwr

# Fonts
mkdir -p ~/.local/share/fonts/noto && wget -qO- "https://noto-website-2.storage.googleapis.com/pkgs/Noto-hinted.zip" | bsdtar -xvf- -C ~/.local/share/fonts/noto

npm install --global generator-code \
gulp-cli \
vsce \
@vue/cli \
yo

reboot
