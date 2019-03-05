#!/bin/bash

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

cd ~/src
git clone git@github.com:xaverh/st.git
cd st
makepkg -Ccsfi

cd ~/src
git clone git@github.com:xaverh/ttf-input.git
cd ttf-input
makepkg -Ccsfi

mkdir -p ~/.local/share/systemd/user
cp ~/etc/nolink/systemd/user/{slock,ssh-agent}.service ~/.local/share/systemd/user
systemctl enable --user slock
systemctl enable --user ssh-agent

systemctl enable --user clipmenud

systemctl enable devmon@$USER.service

ln -s ~/.local/share/applications/mimeapps.list ~/.config/mimeapps.list

code \
--install-extension bierner.markdown-checkbox \
--install-extension bierner.markdown-emoji \
--install-extension bierner.markdown-footnotes \
--install-extension bierner.markdown-mermaid \
--install-extension cg-cnu.vscode-remind-me \
--install-extension chenxsan.vscode-standardjs \
--install-extension eg2.ts-tslint \
--install-extension eg2.vscode-npm-script \
--install-extension hoovercj.vscode-settings-cycler \
--install-extension idleberg.emoji-code \
--install-extension James-Yu.latex-workshop \
--install-extension ms-vscode.cpptools \
--install-extension ms-vscode.Go \
--install-extension msjsdiag.debugger-for-chrome \
--install-extension nhoizey.gremlins \
--install-extension octref.vetur  \
--install-extension trixnz.vscode-lua \
--install-extension twxs.cmake \
--install-extension wmaurer.change-case \
--install-extension xaver.clang-format \
--install-extension xaver.theme-qillqaq \
--install-extension xaver.theme-ysgrifennwr

npm install --global generator-code gulp-cli typescript vsce @vue/cli yo

reboot
