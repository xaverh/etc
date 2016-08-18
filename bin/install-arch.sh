sudo pacman -Syyu --noconfirm
sudo pacman -Sq --noconfirm wget vlc chromium obconf dmenu i3 unzip git tint2 pcmanfm xarchiver gvfs p7zip unrar zsh rxvt-unicode vim texlive-most clang viewnior lxappearance-obconf gtk-engine-murrine compton slock nitrogen volumeicon pulseaudio conky

chsh -s /bin/zsh

cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
~/.dropbox-dist/dropboxd
