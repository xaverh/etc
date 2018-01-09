#!/usr/bin/sh

timedatectl set-ntp 1

cd
mkdir ~/.ssh
mkdir ~/var
mkdir ~/tmp
git clone git@github.com:xaverh/etc.git
mkdir -p ~/.config/Code/User/workspaceStorage
rm .bashrc .bash_logout .bash_profile
cd ~/etc
stow -v !(nolink)
git submodule init
git submodule update

mkdir ~/tmp
cd ~/tmp
gpg --recv-keys --keyserver hkp://pgp.mit.edu 1EB2638FF56C0C53
curl "https://aur.archlinux.org/cgit/aur.git/snapshot/cower.tar.gz" | tar xzf -
cd cower
makepkg -Ccsfi
cd ~/tmp
curl "https://aur.archlinux.org/cgit/aur.git/snapshot/pacaur.tar.gz" | tar xzf -
cd aurutils
makepkg -Ccsfi

sudo pacman -Syu

pacaur -S spotify sgi-fonts conan dropbox toilet cava-git google-chrome wire-desktop-bin visual-studio-code clipmenu neofetch openvpn-update-systemd-resolved simple-mtpfs

cd ~/src
git clone git@github.com:xaverh/dwm.git
cd dwm
makepkg -Ccsfi

cd ~/src
git clone git@github.com:xaverh/st.git
cd st
tic -sx st.info
makepkg -Ccsfi

cd ~/src
git clone git@github.com:xaverh/ttf-input.git
cd ttf-input
makepkg -Ccsfi

mkdir -p ~/.local/share/systemd/user
cp ~/etc/nolink/systemd/user/{dwmstatus,slock,ssh-agent}.service ~/.local/share/systemd/user
systemctl enable --user slock
systemctl enable --user ssh-agent

# Dropbox
systemctl --user enable dropbox
# systemctl --user enable mpd

systemctl --user enable clipmenud

systemctl enable devmon@$USER.service

ln -s ~/.local/share/applications/mimeapps.list ~/.config/mimeapps.list

reboot
