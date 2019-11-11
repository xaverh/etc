#!/usr/bin/zsh

# Installing openSUSE

## Requirements
su
zypper in systemd-container

## Prepare disk
### SSD?
blkdiscard /dev/sda

# encryption?
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda
# else
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8304 /dev/sda

mkfs.fat -F32 /dev/sda1

## encrytption?
cryptsetup benchmark
### SSD?
cryptsetup -y -v --use-random --align-payload=8192 --type luks2 --OPTIONS luksFormat /dev/sda2
### else
cryptsetup -y -v --use-random --type luks2 --OPTIONS luksFormat /dev/sda2
### SSD?
cryptsetup --allow-discards --persistent open /dev/sda2 cryptroot
### else
cryptsetup open /dev/sda2 cryptroot
cryptsetup luksDump /dev/sda2 | grep UUID > ~/cryptdump.txt
#### save UUID: b4a1511b-6e52-4abe-9c45-8578752ac0d8
mkfs.btrfs /dev/mapper/cryptroot | grep UUID > ~/btrfsdump.txt
#### save UUID: 3c6f94ed-f4c6-49f8-a71f-d9fdb6b5d005

## else
mkfs.btrfs /dev/sda2
#### save UUID: 877002d8-f595-4ea6-89ef-a5caec033303

## enc?
mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt
## else
mount -o compress-force=zstd:6,noatime /dev/sda2 /mnt

btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@/home
mkdir /mnt/@/var
chattr +C /mnt/@/var
mkdir -p /mnt/@/var/lib/iwd
mkdir -p /mnt/@/efi
mkdir -p /mnt/@/zypp/repos.d

# TODO: import zypp.conf

btrfs subvolume set-default /mnt/@

cd /
umount /mnt

## enc?
mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt
## else
mount -o compress-force=zstd:6,noatime /dev/sda2 /mnt

mount /dev/sda1 /mnt/efi

zypper -R /mnt ar -c /etc/zypp/repos.d/repo-oss.repo
zypper -R /mnt ar -c /etc/zypp/repos.d/repo-non-oss.repo
zypper -R /mnt ar -c /etc/zypp/repos.d/repo-update.repo
zypper -R /mnt ar -c -p 50 -f http://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Tumbleweed/packman.repo
rpm --root /mnt --import https://packages.microsoft.com/keys/microsoft.asc
cat > /mnt/etc/zypp/repos.d/vscode.repo <<"EOF"
[code]
name=Visual Studio Code
enabled=1
autorefresh=1
baseurl=https://packages.microsoft.com/yumrepos/vscode
type=rpm-md
priority=150
gpgcheck=1
gpgkey=https://packages.microsoft.com/keys/microsoft.asc

EOF

rpm --root /mnt --import https://dl.google.com/linux/linux_signing_key.pub
cat > /mnt/etc/zypp/repos.d/google-chrome.repo <<"EOF"
[google-chrome]
name=google-chrome
enabled=1
autorefresh=1
baseurl=http://dl.google.com/linux/chrome/rpm/stable/x86_64
type=rpm-md
priority=160
keeppackages=0

EOF

# DVD ?
zypper -R /mnt ar -c -f -p 200 "http://download.videolan.org/pub/vlc/SuSE/Tumbleweed/SuSE.repo"
zypper -R /mnt ar -f -p 120 "https://download.opensuse.org/repositories/home:/xha/openSUSE_Tumbleweed/home:xha.repo"

# btrfsmaintenance: https://bugzilla.suse.com/show_bug.cgi?id=1063638#c73
zypper -R /mnt al texlive-\*-doc \*-lang \*cantarell\* grub2 lightdm plymouth google-droid-fonts google-roboto-fonts adobe-source\*-fonts texlive-plex\* liberation-fonts syslinux wireless-tools ucode-amd tigervnc gnome-online-accounts noto-sans-fonts snapper screen samba nano btrfsmaintenance smartmontools PackageKit\* wicked\* maim zypper-aptitude \*-bash-completion xdm dejavu-fonts google-noto-fonts-doc gnu-free-fonts stix-fonts tcsh texlive\*fonts ghostscript-fonts\* patterns-fonts-fonts_opt opensuse-welcome xorg-x11-fonts intlfonts-euro-bitmap-fonts xorg-x11-Xvnc graphviz-gnome yast2-fonts  distribution-logos-openSUSE-Tumbleweed systemd-icon-branding-openSUSE yast2-fonts mpv-plugin-mpris wallpaper-branding-openSUSE hicolor-icon-theme-branding-openSUSE gtk2-branding-openSUSE soundtheme-freedesktop joe gtk2-branding-upstream libqt5-qtstyleplugins-platformtheme-gtk2 libqt5-qtbase-platformtheme-gtk3 gtk3-branding-upstream w3m weechat-spell poppler\* wol inxi yast2-online-update-configuration git-gui yast2 yast2-users btrfs-progs

zypper in -t pattern yast2_basis

zypper -R /mnt al -t pattern fonts_opt
zypper -R /mnt al numlockx dvd+rw-tools

zypper -R /mnt ref

zypper -R /mnt in --auto-agree-with-licenses \
AdobeICCProfiles \
alacritty \
bluez-auto-enable-devices \
bluez-firmware \
clang \
clipmenu \
clipnotify \
cmake \
code \
discord \
dmz-icon-theme-cursors \
flac \
flameshot \
gimp \
git-core \
go \
google-chrome-stable \
gstreamer-plugins-bad \
gstreamer-plugins-good \
gstreamer-plugins-libav \
gstreamer-plugins-ugly \
gstreamer-plugins-ugly-orig-addon \
gtk\?-immodule-xim \
herbstluftwm \
i3lock \
kernel-default \
lemonbar \
lua53 \
MozillaFirefox \
mpv \
mupdf \
ncdu \
nnn \
nodejs \
noto-coloremoji-fonts \
noto-sans-balinese-fonts \
noto-sans-bengali-fonts \
noto-sans-bengali-ui-fonts \
noto-sans-cuneiform-fonts \
noto-sans-deseret-fonts \
noto-sans-khmer-fonts \
noto-sans-myanmar-fonts \
noto-sans-shavian-fonts \
noto-sans-sinhala-fonts \
noto-sans-symbols-fonts \
noto-sans-taitham-fonts \
noto-sans-tamil-fonts \
noto-serif-bengali-fonts \
noto-serif-khmer-fonts \
noto-serif-myanmar-fonts \
noto-serif-sinhala-fonts \
noto-serif-tamil-fonts \
openssh-askpass-gnome \
opus-tools \
patterns-base-enhanced_base \
patterns-base-minimal_base \
patterns-base-x11 \
patterns-base-x11_enhanced \
patterns-desktop-multimedia \
pavucontrol \
permissions-zypp-plugin \
pulseaudio \
pulseaudio \
pulseaudio-module-bluetooth \
pulseaudio-module-x11 \
rofi \
schwammerl \
sent \
sgi-bitmap-fonts \
simple-mtpfs \
steam \
steamtricks \
stow \
strawberry \
sxiv \
systemd-container \
telegram-desktop \
tmux \
unrar \
unzip \
weechat \
wmctrl \
xclip \
xev \
xf86-video-intel \
xinit \
xinput \
xrandr \
xset \
youtube-dl \
zip \
zsh \
zstd

zypper -R /mnt in --auto-agree-with-licenses iw iwd libvulkan_intel gstreamer-plugins-vaapi intel-media-driver libdvdcss2 numlockx abcde broadcom-wl

mkdir -p /mnt/etc/systemd/network/
cat > /mnt/etc/systemd/network/10-wireless.network <<"EOF"
[Match]
Name=wl*

[Network]
DHCP=true
IPv6AcceptRA=true
IPv6PrivacyExtensions=true

[DHCP]
UseDNS=false
UseNTP=false
RouteMetric=10

[IPv6AcceptRA]
UseDNS=false

EOF


mkdir -p /mnt/etc/systemd/resolved.conf.d
cat > /mnt/etc/systemd/resolved.conf.d/10-DNSSEC.conf <<"EOF"
[Resolve]
DNSSEC=false

EOF

cat > /mnt/etc/systemd/resolved.conf.d/20-1.1.1.1.conf <<"EOF"
[Resolve]
DNS=1.1.1.1 2606:4700:4700::1111
FallbackDNS=1.0.0.1 2606:4700:4700::1001

EOF

ln -sf /run/systemd/resolve/stub-resolv.conf /mnt/etc/resolv.conf

ln -s /mnt/usr/share/systemd/tmp.mount /mnt/etc/systemd/system/tmp.mount

## enc?
cat > /mnt/etc/dracut.conf.d/dracut.conf <<"EOF"
hostonly="yes"
add_dracutmodules+="crypt"

EOF

mkdir -p /mnt/etc/kernel
## enc?
cat > /mnt/etc/kernel/cmdline <<"EOF"
root=UUID=3c6f94ed-f4c6-49f8-a71f-d9fdb6b5d005 rd.luks.uuid=b4a1511b-6e52-4abe-9c45-8578752ac0d8 rd.luks.crypttab=0 rw rd.lvm=0 rd.dm=0 rd.md=0 rootflags=defaults,noatime,compress-force=zstd:6,ssd i915.fastboot=1

EOF
## else
cat > /mnt/etc/kernel/cmdline <<"EOF"
root=UUID=877002d8-f595-4ea6-89ef-a5caec033303 rd.luks=0 rw rd.lvm=0 rd.dm=0 rd.md=0 rootflags=defaults,noatime,compress-force=zstd:6,ssd i915.fastboot=1

EOF

sudo mkdir -p /mnt/usr/local/lib/systemd/user/
cat > /mnt/usr/local/lib/systemd/user/ssh-agent.service <<"EOF"
[Unit]
Description=SSH key agent

[Service]
Type=simple
Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket
ExecStart=/usr/bin/ssh-agent -D -a $SSH_AUTH_SOCK

[Install]
WantedBy=default.target

EOF

cat > /mnt/etc/udev/rules.d/backlight.rules <<"EOF"
ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_brightness", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"
ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_brightness", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"

EOF

cat > /mnt/usr/local/lib/systemd/system/i3lock@.service <<"EOF"
[Unit]
Description=Lock X session using i3lock for user %i
Before=sleep.target

[Service]
User=%i
Environment=DISPLAY=:0
ExecStartPre=/usr/bin/xset dpms force suspend
ExecStart=/usr/bin/i3lock -n -c 1e1e1e

[Install]
WantedBy=sleep.target

EOF

cat > /mnt/etc/X11/xorg.conf.d/20-dontzap.conf <<"EOF"
Section "ServerFlags"
             Option "DontVTSwitch" "True"
             Option "DontZap"      "True"
EndSection

EOF

cat > /mnt/etc/X11/xorg.conf.d/30-touchpad.conf <<"EOF"
Section "InputClass"
	Identifier "devname"
	Driver "libinput"
	MatchIsTouchpad "on"
	Option "NaturalScrolling" "true"
	Option "HorizontalScrolling" "0"
        Option "Tapping" "off"
EndSection

EOF

cat > /mnt/etc/X11/xorg.conf.d/31-pointer.conf <<"EOF"
Section "InputClass"
	Identifier "devname"
	Driver "libinput"
	MatchIsPointer "on"
	Option "NaturalScrolling" "false"
EndSection

EOF

cd /
systemd-nspawn -D /mnt passwd root

systemd-nspawn -D /mnt chsh -s /usr/bin/zsh

systemd-nspawn -bD /mnt

## within

localectl set-locale LANG=en_US.UTF-8
localectl set-x11-keymap us pc104 altgr-intl "compose:menu"
localectl set-x11-keymap de apple_laptop mac_nodeadkeys compose:rwin-altgr
timedatectl set-ntp true
timedatectl set-timezone Europe/Berlin
hostnamectl set-hostname andermatt
systemctl enable --now systemd-resolved.service
systemctl enable --now systemd-timesyncd.service

# Add user in YaST, add to groups wheel,systemd-journal,video; /usr/bin/zsh as shell
useradd -m -N -g users -G video,systemd-journal -s /usr/bin/zsh

systemctl enable --now i3lock@xha.service

bootctl install

mkinitrd
kernel-install add `uname -r` /boot/vmlinuz-`uname -r`

# ???
systemctl set-default graphical.target

systemctl disable wicked.service
systemctl enable systemd-networkd.service
systemctl enable iwd.service
systemctl enable btrfs-trim.timer
systemctl enable firewalld.service

iw dev wlan0 set power_save off

### As user:
systemctl --user enable ssh-agent.service
systemctl --user enable clipmenud.service

# Set up wireless

xdg-mime default mupdf-gl.desktop application/pdf
xdg-mime default mupdf-gl.desktop application/vnd.comicbook-rar
xdg-mime default mupdf-gl.desktop application/vnd.comicbook+zip
xdg-mime default mupdf-gl.desktop application/epub+zip
xdg-mime default mupdf-gl.desktop application/x-cb7
xdg-mime default sxiv.desktop image/jpeg
xdg-mime default sxiv.desktop image/png
xdg-mime default sxiv.desktop image/gif
xdg-mime default sxiv.desktop image/tiff

npm -g i @vue/cli generator-code gulp-cli sass vsce yo

reboot