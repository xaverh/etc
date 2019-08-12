#!/bin/zsh

# Installing openSUSE

## Requirements
sudo -s
zypper in systemd-container

## Prepare disk

### SSD?
blkdiscard /dev/sda

sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda

mkfs.fat -F32 /dev/sda1

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

mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt

btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@/home
mkdir /mnt/@/usr
btrfs subvolume create /mnt/@/usr/local
mkdir -p /mnt/@/etc/systemd
btrfs subvolume create /mnt/@/etc/dracut.conf.d
btrfs subvolume create /mnt/@/etc/kernel
btrfs subvolume create /mnt/@/etc/systemd/network
btrfs subvolume create /mnt/@/etc/systemd/resolved.conf.d
btrfs subvolume create /mnt/@/etc/sudoers.d
btrfs subvolume create /mnt/@/etc/systemd/system/getty@tty1.service.d
btrfs subvolume create /mnt/@/etc/zypp/repos.d
mkdir /mnt/@/var
chattr +C /mnt/@/var
mkdir /mnt/@/var/lib
btrfs subvolume create /mnt/@/var/lib/iwd
mkdir -p /mnt/@/boot/efi

mkdir -p /mnt/snapshots/-
mkdir /mnt/snapshots/home
mkdir /mnt/snapshots/usr-local
mkdir /mnt/snapshots/etc-dracut.conf.d
mkdir /mnt/snapshots/etc-kernel
mkdir /mnt/snapshots/etc-systemd-network
mkdir /mnt/snapshots/etc-systemd-resolved.conf.d
mkdir /mnt/snapshots/var-lib-iwd
mkdir /mnt/snapshots/etc-sudoers.d
mkdir /mnt/snapshots/etc-systemd-system-getty\x40tty1.service.d
mkdir /mnt/snapshots/etc-zypp-repos.d

btrfs subvolume set-default /mnt/@

cd /
umount /mnt
mount -o compress-force=zstd:6,noatime /dev/mapper/cryptroot /mnt
mount /dev/sda1 /mnt/boot/efi

zypper -R /mnt ar -c /etc/zypp/repos.d/repo-oss.repo
zypper -R /mnt ar -c /etc/zypp/repos.d/repo-non-oss.repo
zypper -R /mnt ar -c /etc/zypp/repos.d/repo-update.repo
zypper -R /mnt ar -c -p 50 -f http://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Tumbleweed/packman.repo
zypper -R /mnt ref
zypper -R /mnt al cantarell-fonts grub2 lightdm plymouth syslinux wireless-tools ucode-amd tigervnc gnome-online-accounts google-droid-fonts google-roboto-fonts awesome WindowMaker noto-sans-fonts compiz snapper

zypper -R /mnt in patterns-base-minimal_base patterns-base-enhanced_base zsh tmux iw iwd kernel-default


cat > /mnt/etc/dracut.conf.d/dracut.conf <<EOF
hostonly="yes"
add_dracutmodules+="crypt"

EOF

cat > /mnt/etc/kernel/cmdline <<EOF
root=UUID=afe2d9f4-b15b-4f12-a7b0-758a160a5dec rd.luks.uuid=b4a1511b-6e52-4abe-9c45-8578752ac0d8 rd.luks.crypttab=0 rw rd.lvm=0 rd.dm=0 rd.md=0 rootflags=defaults,noatime,compress-force=zstd:6,ssd  i915.fastboot=1

EOF


cat > /mnt/etc/systemd/network/10-wireless.network <<EOF
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



cat > /mnt/etc/systemd/resolved.conf.d/10-DNSSEC.conf <<EOF
[Resolve]
DNSSEC=false

EOF

cat > /mnt/etc/systemd/resolved.conf.d/20-1.1.1.1.conf <<EOF
[Resolve]
DNS=1.1.1.1 2606:4700:4700::1111
FallbackDNS=1.0.0.1 2606:4700:4700::1001

EOF

ln -sf /run/systemd/resolve/stub-resolv.conf /mnt/etc/resolv.conf



mkdir -p /mnt/usr/local/lib/systemd/{system,user}

cat > /mnt/usr/local/lib/systemd/system/tmp.mount <<EOF
[Unit]
Description=Temporary Directory (/tmp)
Documentation=man:hier(7)
Documentation=https://www.freedesktop.org/wiki/Software/systemd/APIFileSystems
ConditionPathIsSymbolicLink=!/tmp
DefaultDependencies=no
Conflicts=umount.target
Before=local-fs.target umount.target
After=swap.target

[Mount]
What=tmpfs
Where=/tmp
Type=tmpfs
Options=mode=1777,strictatime,nosuid,nodev

EOF

cat > /mnt/usr/local/lib/systemd/user/ssh-agent.service <<EOF
[Unit]
Description=SSH key agent

[Service]
Type=simple
Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket
ExecStart=/usr/bin/ssh-agent -D -a \$SSH_AUTH_SOCK

[Install]
WantedBy=default.target

EOF

cd /
systemd-nspawn -D /mnt passwd root

systemd-nspawn -D /mnt chsh -s /bin/zsh

systemd-nspawn -bD /mnt

## within

bootctl install --path /boot/efi
localectl set-locale LANG=en_US.UTF-8
localectl set-x11-keymap us pc104 altgr-intl "terminate:ctrl_alt_bksp,compose:menu"
timedatectl set-ntp true
timedatectl set-timezone Europe/Berlin
hostnamectl set-hostname andermatt
systemctl enable --now systemd-networkd.service
systemctl enable --now systemd-resolved.service
systemctl enable --now systemd-timesyncd.service
systemctl enable iwd.service
mkinitrd
kernel-install add `uname -r` /boot/vmlinuz-`uname -r`

# Add user in YaST, add to group wheel
chsh -s /bin/zsh xha

zypper in patterns-base-x11 fvwm2 rxvt-unicode strawberry steam steamtricks gimp geeqie mupdf youtube-dl telegram-desktop discord weechat lua53 nodejs neofetch maim zip stow MozillaFirefox mpv git-core sxiv gstreamer-plugins-bad gstreamer-plugins-ugly gstreamer-plugins-ugly-orig-addon gstreamer-plugins-libav pcmanfm-qt elementary-icon-theme -wicked -xdm pcmanfm-qt ncdu patterns-desktop-multimedia flac pulseaudio pulseaudio-module-x11

zypper al wicked xdm

systemctl enable btrfs-trim.timer

rpm --import https://packages.microsoft.com/keys/microsoft.asc
cat > /etc/zypp/vscode.repo <<EOF
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
zypper in code

rpm --import https://dl.google.com/linux/linux_signing_key.pub
cat > /etc/zypp/repos.d/google-chrome.repo <<EOF
[google-chrome]
name=google-chrome
enabled=1
autorefresh=1
baseurl=http://dl.google.com/linux/chrome/rpm/stable/x86_64
type=rpm-md
priority=160
keeppackages=0

EOF
zypper in google-chrome

zypper ar --priority 120 "https://download.opensuse.org/repositories/home:/xha/openSUSE_Tumbleweed/home:xha.repo"

zypper in clipmenu clipnotify

# Intel: libvulkan_intel gstreamer-plugins-vaapi
# NVIDIA:
# AMD:

### As user:
systemctl --user enable ssh-agent.service

cat > /tmp/local <<EOF
Defaults insults
Defaults !targetpw
Defaults passwd_tries=2

ALL    ALL=(:) ALL
root   ALL=(ALL) ALL
%wheel ALL=(ALL) ALL

EOF

sudo visudo -c -f /tmp/local && sudo chown root:root /tmp/local && sudo chmod 440 /tmp/local && sudo mv /tmp/local /etc/sudoers.d/
sudo passwd -l root
