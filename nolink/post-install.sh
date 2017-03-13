#!/usr/bin/sh

timedatectl set-ntp 1

cd
git clone git@github.com:xaverh/etc.git

mkdir ~/tmp
cd ~/tmp
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/aurutils.tar.gz" | tar xzf -
cd aurutils
makepkg -si

sudo cp ~/src/dotfiles/nolink/etc/pacman.d/aur /etc/pacman.d/aur
sudo chmod 644 /etc/pacman.d/aur

# Include AUR into pacman.conf
echo "Include = /etc/pacman.d/aur" | sudo tee --append /etc/pacman.conf > /dev/null

sudo install -d /var/cache/pacman/aur -o $USER
repo-add /var/cache/pacman/aur/aur.db.tar

sudo pacman -Syu

aursync -un spotify sgi-fonts urxvtcd conan dropbox farbfeld toilet cava-git google-chrome wire-desktop-bin visual-studio-code playerctl clipmenu neofetch

sudo pacman -Syu

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

# Dropbox
systemctl --user enable dropbox
# systemctl --user enable mpd

reboot
