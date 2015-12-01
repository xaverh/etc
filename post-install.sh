#!/usr/bin/sh

# besser: in /etc/X11/Xresources:

echo "Xft.dpi: 96
Xft.antialias: true
Xft.hinting: true
Xft.hintstyle: hintslight
Xft.rgba: rgb
Xft.lcdfilter: lcddefault
" | sudo tee /etc/X11/Xresources > /dev/null

gsettings "set" "org.gnome.settings-daemon.plugins.xsettings" "hinting" "slight"
gsettings "set" "org.gnome.settings-daemon.plugins.xsettings" "antialiasing" "rgba"

# sudo yum-config-manager --add-repo=http://download.opensuse.org/repositories/home:/satya164:/elegance-colors/Fedora_`rpm -E %fedora`/home:satya164:elegance-colors.repo

# sudo yum-config-manager --add-repo=http://negativo17.org/repos/fedora-handbrake.repo

# sudo yum-config-manager --add-repo=http://download.opensuse.org/repositories/home:/paolorotolo:/numix/Fedora_`rpm -E %fedora`/home:paolorotolo:numix.repo

# Fedora
sudo dnf -y install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-`rpm -E %fedora`.noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-`rpm -E %fedora`.noarch.rpm http://linuxdownload.adobe.com/adobe-release/adobe-release-`uname -m`-1.0-1.noarch.rpm http://rpm.livna.org/livna-release.rpm

sudo dnf upgrade -y

# sudo dnf -y install tint2 clipit xfce4-power-manager pnmixer \
# rxvt-unicode-256color-ml conky dmenu catfish Thunar tumbler viewnior \
# gimp transmission libreoffice zathura-plugins-all gmrun arandr \
# xfce4-notifyd nitrogen slim scrot gtk-murrine-engine ghc lua golang \
# golang-vim ghc libdvdcss zsh  texlive-scheme-medium gvim vim flash-plugin \
# unrar gstreamer1-plugins-bad-freeworld gstreamer1-libav \
# gstreamer1-plugins-ugly gstreamer1-plugins-bad-free-extras \
# gstreamer1-plugins-good-extras vlc faac flac p7zip hunspell-de \
# mythes-de wget samba freetype-freeworld  vim obconf lxappearance obmenu \
# network-manager-applet thunderbird firewall-config slock htop cups \
# cups-bjnp python-smbc avahi system-config-printer gutenprint-cups \
# rdesktop claws-mail-plugins clang golang-vet golang-godoc @c-development
### hsetroot cb-compositor cb-exit

sudo apt-get install adobe-flashplugin git zsh texlive texlive-latex-extra unrar gimp faac faad p7zip fonts-linuxlibertine clang clang-format-3.7 lua5.3 golang xclip network-manager-openconnect-gnome

sudo dnf -y install zsh texlive-scheme-medium vim-enhanced vim-X11 \
flash-plugin gstreamer1-plugins-bad-freeworld \
gstreamer1-libav gstreamer1-plugins-ugly \
gstreamer1-plugins-bad-free-extras gstreamer1-plugins-good-extras \
gimp faac flac evince-djvu evince-dvi \
samba  linux-libertine-fonts \
linux-libertine-biolinum-fonts clang golang golang-godoc \
golang-vet freetype-freeworld lato-fonts ghc @c-development @gnome-games \
transcode kernel-devel @"Development Libraries" glibc-static libstdc++-static \
libdvdcss fuse-exfat cmake gnome-tweak-tool gnome-kra-ora-thumbnailer \
gnome-nds-thumbnailer gnome-epub-thumbnailer gtk-murrine-engine cabextract lzip \
p7zip p7zip-plugins unrar libreoffice-langpack-de

# openSUSE

# http://www.opensuse-community.org

sudo zypper install texlive-scheme-full gvim +pattern:devel_java faad2 faac \
	flac evince-plugin-"*" llvm-clang go go-doc ghc +pattern:devel_C_C++ \
	transcode


# thunderbird vlc  raw-thumbnailer clementine calibre gnome-shell-theme-elegance-colors  gstreamer-plugins-{bad,base,good,ugly,bad-free,good-extras,bad-nonfree,bad-free-extras} numix-icon-theme numix-icon-theme-circle

# sudo zypper install libdvdcss2 texlive-scheme-medium \
# gstreamer-plugins-good-extra faac flac p7zip linux-libertine-fonts \
# llvm-clang go go-vim ghc gcc-c++ nautilus-extension-dropbox

# Samba:
sudo /sbin/setsebool -P samba_enable_home_dirs on
sudo systemctl enable smb && sudo systemctl enable nmb && sudo systemctl start smb && sudo systemctl start nmb
# Firewall: samba und samba-client zulassen, 8611/tcp für bjnp

# http://rpmfusion.org/Howto/nVidia bzw. akmod-catalyst
# http://www.if-not-true-then-false.com/2014/fedora-20-nvidia-guide/

chsh -s /bin/zsh

# http://wiki.ubuntuusers.de/Canon_Pixma_Scanner # braucht Neustart

# von Beta-Versionen aus immer: yum synchronize-distribution

# artwiz-aleczapka-fonts levien-inconsolata-fonts cmus aria2 dvd+rw-tools wodim
# dunst


gsettings set org.gnome.desktop.interface gtk-theme "Numix Light"
gsettings set org.gnome.desktop.wm.preferences theme "Numix Light"


# To make the change permanent, write vm.swappiness=1 on your /etc/sysctl.conf
# file.
# To make the change permanent, put vm.vfs_cache_pressure=50 on your
# /etc/sysctl.conf file.

echo -e "default-cache-ttl 18000\nmax-cache-ttl 86400\nignore-cache-for-signing" >> ~/.gnupg/gpg-agent.conf
echo "use-agent" >>  ~/.gnupg/gpg.conf

# Cocos2d-X
libX11-devel.x86_64 libXmu-devel.x86_64 libXi-devel.x86_64 libzip-devel.x86_64
libpng12-devel.x86_64 libcurl-devel.x86_64 libsqlite3x-devel.x86_64
libgle-devel.x86_64 glew-devel glfw-devel fontconfig-devel ant
