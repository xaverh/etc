#!/usr/bin/sh

# Installing stuff
sudo pacman -Syyu --noconfirm
sudo pacman -Sq --noconfirm vlc qt4 firefox lxappearance-obconf zsh rxvt-unicode vim texlive-most clang pcmanfm-gtk3 xarchiver compton lua gparted xorg-xkill steam noto-fonts noto-fonts-cjk noto-fonts-emoji screenfetch viewnior jdk8-openjdk dunst pkgfile scrot jsoncpp feh xorg-xfontsel wget adobe-source-code-pro-fonts adobe-source-serif-pro-fonts adobe-source-sans-pro-fonts tint2 dmenu xscreensaver conky ttf-linux-libertine gimp l3afpad zathura-pdf-poppler zathura-ps zathura-djvu zathura-cb libstdc++5 llvm imagemagick lightdm-gtk-greeter-settings accountsservice unrar obmenu awesome slock xautolock

mkdir ~/Software
cd ~/Software
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/cower.tar.gz" | tar xzf -
cd cower
makepkg -si

cd ~/Software
cower -d spotify sgi-fonts urxvtcd numix-frost-themes conan dropbox sprunge 2bwm-git
cowerd spotify
cowerd sgi-fonts
cowerd urxvtcd
cowerd numix-frost-themes
cowerd conan
cowerd dropbox
cowerd sprunge
cowerd 2bwm-git

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
rm -r ~/.config/Code/User
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
ln -s ~/Software/dotfiles/.compton.conf ~
mkdir ~/.fonts
ln -s ~/Dropbox/Fonts/Menlo ~/.fonts
ln -s ~/Dropbox/Fonts/San\ Francisco ~/.fonts
ln -s ~/Software/dotfiles/.local/share/thumbnailers ~/.local/share/
rmdir .config/zathura
ln -s ~/Software/dotfiles/.config/zathura/ ~/.config/
ln -s ~/Software/dotfiles/.xprofile ~

chsh -s /bin/zsh
