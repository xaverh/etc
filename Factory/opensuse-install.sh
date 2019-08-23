#!/bin/zsh

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
--align-payload=8192
cryptsetup --type luks2 --OPTIONS luksFormat /dev/sda2
### SSD?
cryptsetup --allow-discards --persistent open /dev/sda2 cryptroot
### else
cryptsetup open /dev/sda2 cryptroot
cryptsetup luksDump /dev/sda2
#### save UUID: b4a1511b-6e52-4abe-9c45-8578752ac0d8
mkfs.btrfs /dev/mapper/cryptroot
#### save UUID: 3c6f94ed-f4c6-49f8-a71f-d9fdb6b5d005

## else
mkfs.btrfs /dev/sda2
#### save UUID: 877002d8-f595-4ea6-89ef-a5caec033303

## enc?
mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt
## else
mount -o compress-force=zstd:6,noatime /dev/sda2 /mnt

btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@.snapshots
btrfs subvolume create /mnt/@/home
mkdir /mnt/@/usr
btrfs subvolume create /mnt/@/usr/local
mkdir -p /mnt/@/usr/local/lib/systemd/{system,user}
mkdir -p /mnt/@/etc/systemd/system
mkdir -p /mnt/@/etc/zypp
mkdir -p /mnt/@/etc/X11
btrfs subvolume create /mnt/@/etc/dracut.conf.d
btrfs subvolume create /mnt/@/etc/kernel
btrfs subvolume create /mnt/@/etc/systemd/network
btrfs subvolume create /mnt/@/etc/systemd/resolved.conf.d
btrfs subvolume create /mnt/@/etc/systemd/system/getty@tty1.service.d
btrfs subvolume create /mnt/@/etc/zypp/repos.d
btrfs subvolume create /mnt/@/etc/X11/xorg.conf.d
mkdir /mnt/@/var
chattr +C /mnt/@/var
mkdir /mnt/@/var/lib
btrfs subvolume create /mnt/@/var/lib/iwd
mkdir -p /mnt/@/efi

mkdir /mnt/@/.snapshots
mkdir -p /mnt/@.snapshots/-
mkdir /mnt/@.snapshots/home
mkdir /mnt/@.snapshots/usr-local
mkdir /mnt/@.snapshots/etc-dracut.conf.d
mkdir /mnt/@.snapshots/etc-kernel
mkdir /mnt/@.snapshots/etc-systemd-network
mkdir /mnt/@.snapshots/etc-systemd-resolved.conf.d
mkdir /mnt/@.snapshots/var-lib-iwd
mkdir /mnt/@.snapshots/etc-systemd-system-getty\\x40tty1.service.d
mkdir /mnt/@.snapshots/etc-zypp-repos.d
mkdir /mnt/@.snapshots/etc-X11-xorg.conf.d

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
# btrfsmaintenance: https://bugzilla.suse.com/show_bug.cgi?id=1063638#c73
zypper -R /mnt al \*cantarell\* grub2 lightdm plymouth syslinux wireless-tools ucode-amd tigervnc gnome-online-accounts google-droid-fonts google-roboto-fonts awesome WindowMaker noto-sans-fonts compiz snapper xdm \*-lang screen samba nano btrfsmaintenance smartmontools PackageKit\* wicked\*
zypper -R /mnt ref

zypper -R /mnt in --auto-agree-with-licenses patterns-base-minimal_base patterns-base-enhanced_base zsh tmux iw iwd

## enc?
cat > /mnt/etc/dracut.conf.d/dracut.conf <<"EOF"
hostonly="yes"
add_dracutmodules+="crypt"

EOF

## enc?
cat > /mnt/etc/kernel/cmdline <<"EOF"
root=UUID=afe2d9f4-b15b-4f12-a7b0-758a160a5dec rd.luks.uuid=b4a1511b-6e52-4abe-9c45-8578752ac0d8 rd.luks.crypttab=0 rw rd.lvm=0 rd.dm=0 rd.md=0 rootflags=defaults,noatime,compress-force=zstd:6,ssd i915.fastboot=1

EOF
## else
cat > /mnt/etc/kernel/cmdline <<"EOF"
root=UUID=877002d8-f595-4ea6-89ef-a5caec033303 rd.luks=0 rw rd.lvm=0 rd.dm=0 rd.md=0 rootflags=defaults,noatime,compress-force=zstd:6,ssd i915.fastboot=1

EOF


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

ln -s /usr/share/systemd/tmp.mount /etc/systemd/system/tmp.mount

cd /
systemd-nspawn -D /mnt passwd root

systemd-nspawn -D /mnt chsh -s /bin/zsh

systemd-nspawn -bD /mnt

## within

localectl set-locale LANG=en_US.UTF-8
localectl set-x11-keymap us pc104 altgr-intl "terminate:ctrl_alt_bksp,compose:menu"
timedatectl set-ntp true
timedatectl set-timezone Europe/Berlin
hostnamectl set-hostname andermatt
systemctl enable --now systemd-resolved.service
systemctl enable --now systemd-timesyncd.service


# Add user in YaST, add to groups wheel,systemd-journal, /bin/zsh as shell

rpm --import https://packages.microsoft.com/keys/microsoft.asc
cat > /etc/zypp/repos.d/vscode.repo <<"EOF"
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

rpm --import https://dl.google.com/linux/linux_signing_key.pub
cat > /etc/zypp/repos.d/google-chrome.repo <<"EOF"
[google-chrome]
name=google-chrome
enabled=1
autorefresh=1
baseurl=http://dl.google.com/linux/chrome/rpm/stable/x86_64
type=rpm-md
priority=160
keeppackages=0

EOF

zypper in kernel-default patterns-base-x11 fvwm2 xwd xrandr ImageMagick rxvt-unicode strawberry steam steamtricks gimp geeqie mupdf youtube-dl telegram-desktop discord weechat lua53 nodejs neofetch maim zip stow MozillaFirefox mpv git-core sxiv gstreamer-plugins-bad gstreamer-plugins-ugly gstreamer-plugins-ugly-orig-addon gstreamer-plugins-libav pcmanfm-qt elementary-icon-theme -xdm pcmanfm-qt ncdu patterns-desktop-multimedia flac pulseaudio pulseaudio-module-x11 xev clang cmake libx265-176 go1.12 pulseaudio pulseaudio-module-bluetooth bluez-auto-enable-devices bluez-firmware pavucontrol numlockx xset sgi-bitmap-fonts systemd-container noto-coloremoji-fonts rofi code google-chrome-stable

## DVD?
zypper ar -c -f -p 200 "http://download.videolan.org/pub/vlc/SuSE/Tumbleweed/SuSE.repo"
zypper in libdvdcss2

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



zypper ar --refresh --priority 120 "https://download.opensuse.org/repositories/home:/xha/openSUSE_Tumbleweed/home:xha.repo"

zypper in clipmenu clipnotify wmbubble

# Intel: libvulkan_intel gstreamer-plugins-vaapi
# NVIDIA:
# AMD:

### As user:
systemctl --user enable ssh-agent.service
systemctl --user enable clipmenud.service

# Set up wireless

# first snapshots
sudo mount -o subvol=@.snapshots,compress-force=zstd:6,noatime /dev/sda2 /.snapshots

for i in /.snapshots/*; btrfs subvolume snapshot -r "$(systemd-escape -pu "${i#/.snapshots/}")" "$i"/`date -Is`

reboot
