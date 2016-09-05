sudo dnf -y install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-`rpm -E %fedora`.noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-`rpm -E %fedora`.noarch.rpm http://linuxdownload.adobe.com/adobe-release/adobe-release-`uname -m`-1.0-1.noarch.rpm http://rpm.livna.org/livna-release.rpm

# sudo dnf -y install clipit xfce4-power-manager \
# catfish \
# gimp transmission libreoffice gmrun arandr \
# xfce4-notifyd scrot ghc golang \
# golang-vim ghc gvim vim faac flac p7zip hunspell-de \
# mythes-de samba freetype-freeworld  vim \
# thunderbird firewall-config slock htop cups \
# cups-bjnp python-smbc avahi system-config-printer gutenprint-cups \
# rdesktop claws-mail-plugins golang-godoc
### hsetroot cb-compositor cb-exit

sudo dnf install xclip tint2 pcmanfm conky tumbler tumbler-extras zathura-plugins-all flash-plugin unrar gstreamer1-plugins-bad-freeworld gstreamer1-libav gstreamer1-plugins-ugly gstreamer1-plugins-bad-free-extras gstreamer1-plugins-good-extras vlc viewnior lxappearance-obconf gtk-murrine-engine gtk-unico-engine network-manager-applet seahorse clang libdvdcss texlive-scheme-medium pnmixer gnome-keyring-pam

# Fedora
# sudo yum-config-manager --add-repo=http://negativo17.org/repos/fedora-handbrake.repo
# sudo dnf upgrade -y
# sudo dnf -y install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-`rpm -E %fedora`.noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-`rpm -E %fedora`.noarch.rpm http://linuxdownload.adobe.com/adobe-release/adobe-release-`uname -m`-1.0-1.noarch.rpm http://rpm.livna.org/livna-release.rpm
# sudo dnf -y install zsh texlive-scheme-medium vim-enhanced vim-X11 \
# flash-plugin gstreamer1-plugins-bad-freeworld glib2-devel \
# gstreamer1-libav gstreamer1-plugins-ugly \
# gstreamer1-plugins-bad-free-extras gstreamer1-plugins-good-extras \
# gimp faac flac evince-djvu evince-dvi \
# samba  linux-libertine-fonts \
# linux-libertine-biolinum-fonts clang golang golang-godoc \
# freetype-freeworld lato-fonts ghc @c-development @gnome-games \
# transcode kernel-devel @"Development Libraries" glibc-static libstdc++-static \
# libdvdcss fuse-exfat cmake gnome-tweak-tool gnome-kra-ora-thumbnailer \
# gnome-nds-thumbnailer gnome-epub-thumbnailer cabextract lzip \
# p7zip p7zip-plugins unrar libreoffice-langpack-de
# # Samba:
# sudo /sbin/setsebool -P samba_enable_home_dirs on
# sudo systemctl enable smb && sudo systemctl enable nmb && sudo systemctl start smb && sudo systemctl start nmb
# # Firewall: samba und samba-client zulassen, 8611/tcp für bjnp
# http://rpmfusion.org/Howto/nVidia bzw. akmod-catalyst
# http://www.if-not-true-then-false.com/2014/fedora-20-nvidia-guide/



# GNOME-Shell-Plugins: Dash to dock, Hide top bar, Straight top bar, Transparent top bar



# http://wiki.ubuntuusers.de/Canon_Pixma_Scanner # braucht Neustart

# von Beta-Versionen aus immer: yum synchronize-distribution

# artwiz-aleczapka-fonts levien-inconsolata-fonts cmus aria2 dvd+rw-tools wodim
# dunst

# Numix
# gsettings set org.gnome.desktop.interface gtk-theme "Numix Light"
# gsettings set org.gnome.desktop.wm.preferences theme "Numix Light"


# To make the change permanent, write vm.swappiness=1 on your /etc/sysctl.conf
# file.
# To make the change permanent, put vm.vfs_cache_pressure=50 on your
# /etc/sysctl.conf file.

# echo -e "default-cache-ttl 18000\nmax-cache-ttl 86400\nignore-cache-for-signing" >> ~/.gnupg/gpg-agent.conf
# echo "use-agent" >>  ~/.gnupg/gpg.conf
