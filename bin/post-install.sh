#!/usr/bin/sh

timedatectl set-ntp 1

localectl set-x11-keymap de

# AUR
mkdir ~/aur
cd ~/aur
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/yaah.tar.gz" | tar xzf -
cd yaah
makepkg -si

cd ~/aur
yaah python-patch python-monotonic python-fasteners spotify sgi-fonts urxvtcd conan dropbox sprunge farbfeld toilet cava-git
# Pakete installieren

mkdir ~/src
cd ~/src
git clone https://github.com/xaverh/dotfiles.git

cd ~/src/dotfiles/nolink/dwm
updpkgsums
makepkg -sfi

cd ~/src/dotfiles/nolink/sent-git
updpkgsums
makepkg -sfi

cd ~/src/dotfiles/nolink/dmenu
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

# Dropbox
systemctl --user enable dropbox
systemctl --user enable mpd

# linking stuff
rm -r ~/.config/Code/User || mkdir --parents ~/.config/Code
ln -s ~/src/dotfiles/.config/Code/User ~/.config/Code/User
ln -s ~/src/dotfiles/.clang-format ~
ln -s ~/src/dotfiles/.dircolors ~
ln -s ~/src/dotfiles/.toprc ~
ln -s ~/src/dotfiles/.Xresources ~
xrdb ~/.Xresources
ln -s ~/src/dotfiles/.xinitrc ~
ln -s ~/src/dotfiles/.zlogin ~
ln -s ~/src/dotfiles/.zsh ~
ln -s ~/src/dotfiles/.zshrc ~
ln -s ~/src/dotfiles/.zshenv ~
ln -s ~/src/dotfiles/.zprofile ~
ln -s ~/src/dotfiles/.config/fontconfig ~/.config/
ln -s ~/src/dotfiles/.config/dunst ~/.config/
ln -s ~/src/dotfiles/.config/compton.conf ~/.config/
mkdir --parents ~/.local/share/fonts
ln -s ~/Dropbox/Fonts/Menlo ~/.local/share/fonts
ln -s ~/Dropbox/Fonts/San\ Francisco ~/.local/share/fonts
ln -s ~/src/dotfiles/.config/zathura/ ~/.config/
ln -s ~/src/dotfiles/.vim ~
ln -s ~/src/dotfiles/.npmrc ~
mkdir ~/.npm-packages
mkdir ~/bin
ln -s ~/src/dotfiles/bin/dwm_status ~/bin
mkdir --parents ~/.config/ranger
ln -s ~/src/.config/ranger/rc.conf ~/.config/ranger/

reboot
