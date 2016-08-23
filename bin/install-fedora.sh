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