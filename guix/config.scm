
(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
	     (rnrs lists))

(use-service-modules
 dbus
 desktop
 networking
 ssh
 xorg)

(operating-system
 (kernel linux)
 (initrd (lambda (file-systems . rest)
	   (apply microcode-initrd file-systems
		  #:initrd base-initrd
		  #:microcode-packages (list intel-microcode)
		  rest)))
 (firmware (cons* iwlwifi-firmware
                  %base-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "latitude" #:options '("compose:menu" "rupeesign:4")))
 (host-name "andermatt")
 (users (cons* (user-account
                (name "xha")
                (comment "Xaver Hellauer")
                (group "users")
                (home-directory "/home/xha")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "input" "tty")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "awesome")
         (specification->package "alacritty")
         (specification->package "iwd")
	 (specification->package "mpv")
	 (specification->package "xorg-server")
	 (specification->package "pipewire")
	 (specification->package "xf86-input-libinput")
	 (specification->package "xinit")
         (specification->package "git")
         (specification->package "vim")
         (specification->package "zathura")
         (specification->package "neofetch")
         (specification->package "pfetch")
         (specification->package "btrfs-progs")
         (specification->package "ungoogled-chromium")
         (specification->package "tmux")
	 (specification->package "font-openmoji")
	 (specification->package "unicode-emoji")
         (specification->package "emacs-next")
         ;; (specification->package "emacs-guix")
         (specification->package "nss-certs"))
   %base-packages))
 (services
  (append
   (list (service openssh-service-type)
	 (service network-manager-service-type)
	 (service wpa-supplicant-service-type)
	 (set-xorg-configuration (xorg-configuration (keyboard-layout keyboard-layout))))
	 %base-services))
	 ;(remove (lambda (service) (eq? (service-kind service) gdm-service-type)) %desktop-services)))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (uuid "b4673858-f743-40f0-b87d-1cb1dc9d13af")))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "b30a6bd9-3524-44c2-aa3e-67b415b2b29d"
                 'btrfs))
          (type "btrfs"))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "5F05-7617" 'fat32))
          (type "vfat"))
         %base-file-systems)))
