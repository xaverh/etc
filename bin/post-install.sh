#!/usr/bin/sh

timedatectl set-ntp 1

# AUR
cd /tmp
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/pacaur.tar.gz" | tar xzf -
cd pacaur
makepkg -si

pacaur -Sa spotify sgi-fonts urxvtcd conan dropbox farbfeld toilet cava-git google-chrome wire-desktop-bin visual-studio-code playerctl

mkdir ~/src
cd ~/src
git clone git@github.com:xaverh/dotfiles.git

cd ~/src
git clone git@github.com:xaverh/dwm.git
cd dwm
updpkgsums
makepkg -sfi

cd ~
git clone git@github.com:xaverh/bin.git

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
# systemctl --user enable mpd

# linking stuff, TODO: outsource to stow
rm -r ~/.config/Code/User || mkdir --parents ~/.config/Code
ln -s ~/src/dotfiles/.config/Code/User ~/.config/Code/User
ln -s ~/src/dotfiles/.clang-format ~
ln -s ~/src/dotfiles/.dircolors ~
ln -s ~/src/dotfiles/.toprc ~
ln -s ~/src/dotfiles/.Xresources ~
ln -s ~/src/dotfiles/.xinitrc ~
ln -s ~/src/dotfiles/.zlogin ~
ln -s ~/src/dotfiles/.zsh ~
ln -s ~/src/dotfiles/.zshrc ~
ln -s ~/src/dotfiles/.zshenv ~
ln -s ~/src/dotfiles/.zprofile ~
ln -s ~/src/dotfiles/.config/fontconfig ~/.config/
ln -s ~/src/dotfiles/.config/dunst ~/.config/
mkdir --parents ~/.local/share/fonts
ln -s ~/Dropbox/Fonts/Menlo ~/.local/share/fonts
ln -s ~/Dropbox/Fonts/San\ Francisco ~/.local/share/fonts
ln -s ~/src/dotfiles/.config/zathura/ ~/.config/
ln -s ~/src/dotfiles/.vim ~
ln -s ~/src/dotfiles/.npmrc ~
mkdir ~/.npm-packages
rmdir ~/.config/mpv
ln -s ~/src/dotfiles/.config/mpv/ ~/.config
ln -s ~/src/.gitconfig ~

reboot
