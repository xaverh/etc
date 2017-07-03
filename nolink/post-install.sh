#!/usr/bin/sh

timedatectl set-ntp 1

cd
mkdir ~/.cache
mkdir ~/.mozilla
# ext enc
mkdir ~/.ssh
git clone git@github.com:xaverh/etc.git
mkdir -p ~/.config/Code/User/workspaceStorage
rm .bashrc .bash_logout .bash_profile
cd ~/etc
stow -v !(nolink)
git submodule update

mkdir ~/tmp
cd ~/tmp
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/aurutils.tar.gz" | tar xzf -
cd aurutils
makepkg -Ccsfi

sudo cp ~/etc/nolink/etc/pacman.d/aur /etc/pacman.d/aur
sudo chmod 644 /etc/pacman.d/aur

# Include AUR into pacman.conf
echo "Include = /etc/pacman.d/aur" | sudo tee --append /etc/pacman.conf > /dev/null

sudo install -d /var/cache/pacman/aur -o $USER
repo-add /var/cache/pacman/aur/aur.db.tar

sudo pacman -Syu

aursync -un spotify-stable sgi-fonts urxvtcd conan dropbox farbfeld toilet cava-git google-chrome wire-desktop-bin visual-studio-code clipmenu neofetch aurutils repoctl

repoctl config new /var/cache/pacman/aur/aur.db.tar

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

systemctl enable devmon@$USER.service
# oder
systemctl enable udisks

ln -s ~/.local/share/applications/mimeapps.list ~/.config/mimeapps.list

reboot
