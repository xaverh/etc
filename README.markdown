# Fedora setup

## within the live image

### preparations

- set keymap
- run `lsblk` to find hard drive, in the following I am going to assume `/dev/sda`
- setup networking
- verify if the image has been booted in EFI mode: `ls /sys/firmware/efi/efivars`

### in a root shell

#### optionally clean remainders of old installs

- run `efibootmgr` to remove old EFI boot entries
- on an SSD run `blkdiscard /dev/sda` to force it to discard all blocks

#### format the drive

##### setup with full-disk encryption

```sh
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8309 /dev/sda
mkfs.fat -F32 /dev/sda1

# the '--align-payload' options depends on the drive:
cryptsetup -y --use-random -v --type luks2 --align-payload=8192 luksFormat /dev/sda2
cryptsetup open /dev/sda2 cryptroot

# '--allow-discards' is only needed for SSDs
cryptsetup --allow-discards --persistent open /dev/sda2 cryptroot
cryptsetup luksUUID /dev/sda2
# save the UUID, e.g. 04f7a64c-e13f-4a09-a2bb-afbfc3c45390

mkfs.btrfs /dev/mapper/cryptroot
# save the UUID, e.g. 23ccb92a-f945-4ef6-aecc-e32b46840ee1
```

##### setup without full-disk encryption

```sh
sgdisk -Z -o -n 1:0:+200MiB -t 1:ef00 -n 2:0:0 -t 2:8304 /dev/sda
mkfs.fat -F32 /dev/sda1
mkfs.btrfs /dev/sda2
# save the PARTUUID, e.g. 0c8461cd-db5c-4249-96b8-18451311aab0
```

#### mount and create mount points

For non-encrypted installs replace `/dev/mapper/cryptroot` with actual device file (e.g. `/dev/sda2`)

```sh
mount -o compress-force=zstd:6,lazytime /dev/mapper/cryptroot /mnt
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@/home
mkdir /mnt/@/var
chattr +C /mnt/@/var
mkdir /mnt/@/efi
btrfs subvolume set-default /mnt/@
umount /mnt
mount -o compress-force=zstd:6,lazytime /dev/mapper/cryptroot /mnt
mount /dev/sda1 /mnt/efi
```

##### configure repositories on live system

```sh
# RPM Fusion et al.
dnf config-manager --enablerepo fedora-cisco-openh264
dnf install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
dnf install https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
dnf install rpmfusion-free-release-tainted rpmfusion-nonfree-release-tainted

# Visual Studio Code
rpm --import https://packages.microsoft.com/keys/microsoft.asc
echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo

# Google Chrome
rpm --import https://dl.google.com/linux/linux_signing_key.pub
echo -e "[google-chrome]\nname=google-chrome\nbaseurl=http://dl.google.com/linux/chrome/rpm/stable/x86_64\nenabled=1\ngpgcheck=1\ngpgkey=https://dl.google.com/linux/linux_signing_key.pub" > /etc/yum.repos.d/google-chrome.repo

# private repo
dnf config-manager --add-repo https://download.opensuse.org/repositories/home:/xha/Fedora_$(rpm -E %fedora)/home:xha.repo

dnf check-update
```

##### optionally define excludes

Some optional packages to exclude in `/etc/dnf/dnf.conf` to avoid pulling them in as weak dependencies:

```ini
exclude=NetworkManager plymouth* PackageKit-gstreamer-plugin abattis-cantarell-fonts fedora-bookmarks dhcp-client mercurial subversion wireless-tools hddtemp xorg-x11-drv-ati xorg-x11-drv-nouveau xorg-x11-drv-intel
```

#### bootstrap base packages

```sh
dnf install --installroot=/mnt --releasever=/ @core zsh glibc-langpack-en vim-enhanced btrfs-progs util-linux-user rpmfusion-free-release-tainted rpmfusion-nonfree-release-tainted sqlite dbus-tools
```

optionally include as needed e.g. `iwd wireless-regdb broadcom-wl iwl6000g2a-firmware cryptsetup`

#### setup base system

```sh
systemd-firstboot --root=/mnt --locale=en_US.UTF-8 --keymap=us --hostname=airolo --setup-machine-id
systemd-nspawn -D /mnt chsh -s /usr/bin/zsh
systemd-nspawn -D /mnt useradd -m -U -G wheel -s /usr/bin/zsh xha
setenforce 0 && systemd-nspawn -D /mnt passwd xha && setenforce 1
```

##### systemd-networkd & systemd-resolved

In `/mnt/etc/systemd/network/05-wired.network`:

```ini
[Match]
Name=e*

[Network]
DHCP=true
IPv6AcceptRA=true
IPv6PrivacyExtensions=kernel

[DHCP]
UseDNS=false
UseNTP=false
RouteMetric=5

[IPv6AcceptRA]
UseDNS=false
```

In `/mnt/etc/systemd/resolved.conf.d/20-1.1.1.1.conf`:

```ini
[Resolve]
FallbackDNS=2606:4700:4700::1111 1.1.1.1 2606:4700:4700::1001 1.0.0.1
Domains=~.
DNSSEC=false
```

For WiFi support in `/mnt/etc/iwd/main.conf`:

```ini
[General]
EnableNetworkConfiguration=true
UseDefaultInterface=true

[Network]
NameResolvingService=systemd
```

In `/etc/systemd/system/iwd.service.d/override.conf`:

```ini
[Unit]
After=systemd-udevd.service network-pre.target
```

###### copy repositories onto target system

```sh
cp -nv /etc/yum.repos.d/* /mnt/etc/yum.repos.d/
cp /etc/dnf/dnf.conf /mnt/etc/dnf/dnf.conf
```

## within the target system

### boot target system

```sh
systemd-nspawn -bD /mnt
```

### install systemd-boot

```sh
sudo bootctl --esp-path=/efi install
```

### configure dracut

In `/etc/dracut.conf.d/local.conf`:

```ini
filesystems+="btrfs"
hostonly="yes"
add_dracutmodules+="crypt"
```

### install packages and kernel

e.g.

```sh
sudo dnf install kernel @base-x @multimedia @firefox google-chrome at code gnome-keyring gimp mpv mpv-mpris youtube-dl ffmpeg telegram-desktop discord flameshot pavucontrol nnn rmlint unrar unzip exfat-utils git nodejs golang lua @c-development man-pages clipmenu clipnotify xclip sent slock google-noto-emoji-color-fonts google-noto-{sans,serif}-tamil-fonts gdouros-aegean-fonts gdouros-aegyptus-fonts gdouros-symbola-fonts dmz-cursor-themes unicode-emoji strawberry awesome mupdf perl-File-MimeInfo groff-perl playerctl tmux rofi rxvt-unicode wireguard-tools iw libdvdcss bluez bluez-tools pulseaudio-module-bluetooth-freeworld steam rawtherapee libva-intel-driver abcde gstreamer1-vaapi libva-intel-hybrid-driver weechat smartmontools f32-backgrounds-base
```

### configure keyboard layout and timezone

e.g.

```sh
localectl set-x11-keymap us pc104 altgr-intl compose:menu,rupeesign:4
localectl set-x11-keymap de apple_laptop mac_nodeadkeys compose:rwin-altgr
timedatectl set-timezone Europe/Berlin
```

### start systemd-networkd, systemd-resolved, systemd-timesyncd, sshd, iwd and fstrim

```sh
sudo ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
systemctl enable --now systemd-networkd.service
systemctl enable --now systemd-resolved.service
systemctl enable --now systemd-timesyncd.service
systemctl enable sshd.socket
systemctl enable iwd.service
systemctl enable fstrim.timer
```

### fix kernel boot line in entry in /efi/loader/entries/\*.conf

```
root=PARTUUID=0c8461cd-db5c-4249-96b8-18451311aab0 rd.luks.crypttab=0 rw rd.lvm=0 rd.md=0 rootflags=defaults,lazytime,compress-force=zstd:6,ssd i915.fastboot=1
root=UUID=23ccb92a-f945-4ef6-aecc-e32b46840ee1 rd.luks.uuid=04f7a64c-e13f-4a09-a2bb-afbfc3c45390 rd.luks.crypttab=0 rw rd.lvm=0 rd.md=0 rootflags=defaults,lazytime,compress-force=zstd:6,ssd i915.fastboot=1
```

### force SELinux to relabel everything

```sh
sudo touch /.autorelabel
```

### boot into new system

```sh
exit
reboot
```

## in the actual booted systemd as user

### start ntp

```sh
timedatectl set-ntp true
```

### setup slock

In `/usr/local/lib/systemd/system/slock@.service`:

```ini
[Unit]
Description=Lock X session and deletes all SSH identities using slock for user %i
Before=sleep.target

[Service]
User=%i
Environment=DISPLAY=:0
ExecStartPre=/usr/bin/xset dpms force suspend
ExecStart=/usr/bin/slock ssh-add -D

[Install]
WantedBy=sleep.target
```

```sh
systemctl enable slock@xha.service
```

### polkit rules

To allow user to start/stop certain systemd-services without password, in `/etc/polkit-1/rules.d/49-nopasswd_limited.rules`:

```js
polkit.addRule(function(action, subject) {
  if (action.id == 'org.freedesktop.systemd1.manage-units') {
    if (
      subject.isInGroup('wheel') &&
      (/wg-quick@mullvad\-[a-z]+[0-9]+.service/.test(action.lookup('unit')) ||
        action.lookup('unit') == 'iwd.service')
    ) {
      var verb = action.lookup('verb')
      if (verb == 'start' || verb == 'stop' || verb == 'restart') {
        return polkit.Result.YES
      }
    }
  }
})

polkit.addRule(function(action, subject) {
  if (
    action.id == 'org.freedesktop.policykit.exec' &&
    action.lookup('program') == '/usr/bin/resolvectl'
  ) {
    return polkit.Result.YES
  }
})
```

### optionally setup autologin

In `/etc/systemd/system/getty@tty1.service.d/override.conf`:

```ini
[Service]
ExecStart=
ExecStart=-/usr/sbin/agetty --autologin xha --noclear %I $TERM
```

### backlight support

To allow group `xha` to change the backlight:

```
ACTION=="add", SUBSYSTEM=="backlight", RUN+="/bin/chgrp xha /sys/class/backlight/%k/brightness"
ACTION=="add", SUBSYSTEM=="backlight", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"
ACTION=="add", SUBSYSTEM=="leds", RUN+="/bin/chgrp xha /sys/class/leds/%k/brightness"
ACTION=="add", SUBSYSTEM=="leds", RUN+="/bin/chmod g+w /sys/class/leds/%k/brightness"
```

### (optionally) disable fn for F1-F12 keys / switch alt and cmd

```sh
echo 'w /sys/module/hid_apple/parameters/fnmode - - - - 2' | sudo tee /etc/tmpfiles.d/rev_fn_key.conf
```

### fix for Dropbox, Visual Studio Code …

```sh
echo 'fs.inotify.max_user_watches=524288' | sudo tee /etc/sysctl.d/95-max-user-watches.conf
```

### default applications

In `/tmp/nnn.desktop`:

```ini
[Desktop Entry]
Name=Nnn
GenericName=File Manager
Comment=minimalist file manager
MimeType=inode/directory;
Exec=/usr/bin/urxvtc -title Nnn -name Nnn -e nnn %u
Icon=/home/xha/.config/Icons/nnn.svg
Type=Application
Categories=System;FileTools;FileManager;Utility;Core;ConsoleOnly
Keywords=File;Manager;Management;Explorer;Launcher
StartupNotify=true
X-GNOME-SingleWindow=false
StartupWMClass=Nnn
```

```sh
xdg-mime default mupdf-gl.desktop application/pdf application/vnd.comicbook+zip application/epub+zip
xdg-mime default sxiv.desktop image/jpeg image/png image/gif image/tiff image/webp image/x-xpmi
xdg-mime default nnn.desktop inode/directory
xdg-mime default mpv.desktop video/mp4 video/webm video/x-matroska
desktop-file-install --rebuild-mime-info-cache --dir="$XDG_DATA_HOME/applications" /tmp/nnn.desktop
xdg-settings set default-url-scheme-handler file nnn.desktop
```

### Node.js and Visual Studio Code packages

```sh
npm -g i @vue/cli generator-code gulp-cli vsce yo

code --install-extension bierner.markdown-checkbox --install-extension bierner.markdown-footnotes --install-extension bierner.markdown-mermaid --install-extension christian-kohler.npm-intellisense --install-extension dbaeumer.vscode-eslint --install-extension eg2.vscode-npm-script --install-extension esbenp.prettier-vscode --install-extension firefox-devtools.vscode-firefox-debug --install-extension James-Yu.latex-workshop --install-extension ms-vscode.cpptools --install-extension ms-vscode.Go --install-extension msjsdiag.debugger-for-chrome --install-extension nhoizey.gremlins --install-extension octref.vetur --install-extension pflannery.vscode-versionlens --install-extension sdras.night-owl --install-extension sdras.vue-vscode-snippets --install-extension trixnz.vscode-lua --install-extension VisualStudioExptTeam.vscodeintellicode --install-extension wmaurer.change-case --install-extension xaver.clang-format --install-extension xaver.theme-qillqaq --install-extension xaver.theme-ysgrifennwr
```

#### bluetooth

In `/etc/pulse/default.pa`:

```
load-module module-switch-on-connect
```

#### change \$ZDOTDIR to use different folder for .zshrc

```sh
echo 'export ZDOTDIR="$HOME"/.config/zsh' | sudo tee -a /etc/zshenv
```

#### deactivate OpenSSH password authentication

In `/etc/ssh/sshd_config.d/99-nopassword.conf` set to `PasswordAuthentication no`.

#### Firefox

```
browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned: amazon
browser.newtabpage.pinned: [null,{"url":"https://smile.amazon.de","label":"@amazon","searchTopSite":true}]
extensions.pocket.enabled: false
```
