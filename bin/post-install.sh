#!/usr/bin/sh

# Installing stuff
sudo pacman -Syyu --noconfirm
sudo pacman -Sq --noconfirm --needed firefox lxappearance-obconf zsh rxvt-unicode vim texlive-most clang xarchiver compton lua gparted xorg-xkill steam noto-fonts noto-fonts-cjk noto-fonts-emoji screenfetch jdk8-openjdk dunst pkgfile scrot jsoncpp feh xorg-xfontsel wget adobe-source-code-pro-fonts adobe-source-serif-pro-fonts adobe-source-sans-pro-fonts tint2 dmenu conky ttf-linux-libertine gimp l3afpad zathura-pdf-poppler zathura-ps zathura-djvu zathura-cb libstdc++5 llvm imagemagick unrar slock xautolock git abs mpd ncmpcpp unzip ttyload thunar thunar-archive-plugin exfat-utils tumbler ffmpegthumbnailer raw-thumbnailer mpv lib32-libpulse lib32-openal lib32-nss lib32-gtk2 lib32-gtk3 lib32-libcanberra lib32-gconf lib32-dbus-glib lib32-libnm-glib lib32-alsa-plugins youtube-dl numlockx npm nodejs mpc p7zip zsh-syntax-highlighting ranger pulseaudio pulseaudio-alsa pamixer alsa-utils bc

# Laptop?
sudo pacman -Sq --noconfirm --needed xf86-input-synaptics acpi wpa_supplicant iw
sudo cp /etc/wpa_supplicant/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
sudo chmod a+r /etc/wpa_supplicant/wpa_supplicant-wlan0.conf
sudo systemctl enable wpa_supplicant@wlan0.service
sudo systemctl start wpa_supplicant@wlan0.service


mkdir ~/aur
cd ~/aur
wget -O - "https://aur.archlinux.org/cgit/aur.git/snapshot/yaah.tar.gz" | tar xzf -
cd yaah
makepkg -si

# sudo cp ~/src/dotfiles/bin/cowerd /usr/local/bin
# sudo chmod 755 /usr/local/bin/cowerd
# sudo pacman-key -r 24B445614FAC071891EDCE49CDBD406AA1AA7A1D
# sudo pacman-key --lsign-key 24B445614FAC071891EDCE49CDBD406AA1AA7A1DM

cd ~/aur
yaah python-patch python-monotonic python-fasteners spotify sgi-fonts urxvtcd conan dropbox sprunge farbfeld toilet

# evtl. benötigte Pakete
# numix-frost-themes

# sudo abs

mkdir ~/src
cd ~/src
git clone https://github.com/xaverh/dotfiles.git
cd ~/src/dotfiles/nolink/dwm
updpkgsums
makepkg -sfi

cd ~/src/dotfiles/nolink/sent-git
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

timedatectl set-ntp 1

localectl set-x11-keymap de

reboot
