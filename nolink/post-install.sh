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

pacaur -S spotify sgi-fonts conan dropbox toilet cava-git google-chrome wire-desktop-bin visual-studio-code clipmenu neofetch openvpn-update-systemd-resolved simple-mtpfs mons

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

code --install-extension James-Yu.latex-workshop --install-extension Telerik.nativescript --install-extension bierner.markdown-checkbox --install-extension bierner.markdown-emoji --install-extension bierner.markdown-footnotes --install-extension bierner.markdown-mermaid --install-extension dbaeumer.vscode-eslint --install-extension eg2.ts-tslint --install-extension eg2.vscode-npm-script --install-extension formulahendry.code-runner --install-extension hoovercj.vscode-settings-cycler --install-extension idleberg.emoji-code --install-extension lukehoban.Go --install-extension ms-python.python --install-extension ms-vscode.cpptools --install-extension ms-vscode.node-debug2 --install-extension msjsdiag.debugger-for-chrome --install-extension nhoizey.gremlins --install-extension octref.vetur --install-extension ravilang.ravi-debug --install-extension satoren.lualint --install-extension twxs.cmake --install-extension wmaurer.change-case --install-extension xaver.clang-format --install-extension xaver.theme-qillqaq --install-extension xaver.theme-ysgrifennwr

npm install --global eslint eslint-config-standard eslint-plugin-html eslint-plugin-import eslint-plugin-node eslint-plugin-promise eslint-plugin-standard eslint-plugin-vue generator-code gulp-cli standard typescript vsce vue-cli yo

reboot
