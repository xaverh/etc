#!/usr/bin/sh

# Installing stuff
sudo pacman -Syyu --noconfirm
sudo pacman -Sq --noconfirm firefox lxappearance-obconf zsh rxvt-unicode vim texlive-most clang xarchiver compton lua gparted xorg-xkill steam noto-fonts noto-fonts-cjk noto-fonts-emoji screenfetch viewnior jdk8-openjdk dunst pkgfile scrot jsoncpp feh xorg-xfontsel wget adobe-source-code-pro-fonts adobe-source-serif-pro-fonts adobe-source-sans-pro-fonts tint2 dmenu conky ttf-linux-libertine gimp l3afpad zathura-pdf-poppler zathura-ps zathura-djvu zathura-cb libstdc++5 llvm imagemagick unrar slock xautolock git abs mpd ncmpcpp unzip ttyload thunar thunar-archive-plugin exfat-utils tumbler ffmpegthumbnailer raw-thumbnailer mpv lib32-libpulse lib32-openal lib32-nss lib32-gtk2 lib32-gtk3 lib32-libcanberra lib32-gconf lib32-dbus-glib lib32-libnm-glib

gpg --recv-keys --keyserver hkp://pgp.mit.edu 1EB2638FF56C0C53

mkdir ~/Software
cd ~/Software
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/cower.tar.gz" | tar xzf -
cd cower
makepkg -si

sudo cp ~/Software/dotfiles/bin/cowerd /usr/local/bin
sudo chmod 755 /usr/local/bin/cowerd

cd ~/Software
cowerd python-patch
cowerd python-monotonic
cowerd python-fasteners
cowerd spotify
cowerd sgi-fonts
cowerd urxvtcd
cowerd numix-frost-themes
cowerd conan
cowerd dropbox
cowerd sprunge

sudo abs
cd ~/Software/dotfiles/nolink/dwm
updpkgsums
makepkg -sfi

## setting up antialiasing
echo "Xft.dpi: 96
Xft.antialias: true
Xft.hinting: true
Xft.hintstyle: hintslight
Xft.rgba: rgb
Xft.lcdfilter: lcddefault
" | sudo tee /etc/X11/Xresources > /dev/null
sudo chmod 644 /etc/X11/Xresources
gsettings "set" "org.gnome.settings-daemon.plugins.xsettings" "hinting" "slight"
gsettings "set" "org.gnome.settings-daemon.plugins.xsettings" "antialiasing" "rgba"

# linking stuff
rm -r ~/.config/Code/User || mkdir --parents ~/.config/Code
ln -s ~/Dropbox/Visual\ Studio\ Code/User ~/.config/Code/User
ln -s ~/Software/dotfiles/.clang-format ~
ln -s ~/Software/dotfiles/.dircolors ~
ln -s ~/Software/dotfiles/.toprc ~
ln -s ~/Software/dotfiles/.Xresources ~
xrdb ~/.Xresources
ln -s ~/Software/dotfiles/.zlogin ~
ln -s ~/Software/dotfiles/.zsh ~
ln -s ~/Software/dotfiles/.zshrc ~
ln -s ~/Software/dotfiles/.zshenv ~
ln -s ~/Software/dotfiles/.zprofile ~
rm -r ~/.config/openbox
ln -s ~/Software/dotfiles/.config/openbox ~/.config/
ln -s ~/Software/dotfiles/.config/tint2 ~/.config/
ln -s ~/Software/dotfiles/.local/share/applications ~/.local/share/
rm -r ~/.config/fontconfig
ln -s ~/Software/dotfiles/.config/fontconfig ~/.config/
ln -s ~/Software/dotfiles/.config/dunst ~/.config/
ln -s ~/Software/dotfiles/.config/compton.conf ~/.config/
mkdir ~/.fonts
ln -s ~/Dropbox/Fonts/Menlo ~/.fonts
ln -s ~/Dropbox/Fonts/San\ Francisco ~/.fonts
ln -s ~/Software/dotfiles/.local/share/thumbnailers ~/.local/share/
rmdir .config/zathura
ln -s ~/Software/dotfiles/.config/zathura/ ~/.config/
ln -s ~/Software/dotfiles/.xprofile ~
ln -s ~/Software/dotfiles/.config/herbstluftwm/ ~/.config/
ln -s ~/Software/dotfiles/.vim ~

timedatectl set-ntp 1

chsh -s /usr/bin/zsh
