(use-modules (gnu)
	     (guix download)
	     (guix build-system font)
	     (guix build-system trivial)
	     (nonguix licenses)
             (gnu packages)
	     (guix packages)
             (gnu packages admin)
             (gnu packages certs)
             (gnu packages chromium)
             (gnu packages compression)
             (gnu packages conky)
             (gnu packages curl)
             (gnu services desktop)
             (gnu packages emacs)
	     (gnu packages emacs-xyz)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages freedesktop)
             (gnu packages gimp)
             (gnu packages image)
             (gnu packages image-viewers)
             (gnu packages linux)
             (gnu packages pdf)
	     (gnu packages pulseaudio)
             (gnu services sound)
             (gnu packages suckless)
             (gnu packages terminals)
             (gnu packages tmux)
             (gnu packages version-control)
             (gnu packages video)
             (gnu packages vim)
             (gnu packages wm)
             (gnu services xorg)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
	     (gnu packages xdisorg)
             (gnu packages xorg)
             (gnu packages xorg))
(use-service-modules networking ssh)
(use-package-modules ssh)

(define-public brave
  (package
   (name "brave")
   (version "1.30.86")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/brave/brave-browser/releases/download/v1.30.86/brave-browser_1.30.86_amd64.deb")
     (sha256
      (base32
       "1jq4x6l754c19cqsw33xb80jksn9zz8lw19hswv5x8hyixrz5hiv"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("patchelf" ,(@ (gnu packages elf) patchelf))
      ("tar" ,(@ (gnu packages base) tar))
      ("xz" ,(@ (gnu packages compression) xz))
      ("binutils" ,(@ (gnu packages base) binutils))))
   (inputs
    `(("libxcomposite" ,(@ (gnu packages xorg) libxcomposite))
      ("libxtst" ,(@ (gnu packages xorg) libxtst))
      ("nss" ,(@ (gnu packages nss) nss))
      ("nspr" ,(@ (gnu packages nss) nspr))
      ("cups" ,(@ (gnu packages cups) cups))
      ("libxrandr" ,(@ (gnu packages xorg) libxrandr))
      ("libxscrnsaver" ,(@ (gnu packages xorg) libxscrnsaver))
      ("alsa-lib" ,(@ (gnu packages linux) alsa-lib))
      ("gcc" ,(@ (gnu packages gcc) gcc-7) "lib")
      ("libxcursor" ,(@ (gnu packages xorg) libxcursor))
      ("libxdamage" ,(@ (gnu packages xorg) libxdamage))
      ("gobject-introspection" ,(@ (gnu packages glib) gobject-introspection))
      ("glib" ,(@ (gnu packages glib) glib))
      ("dbus" ,(@ (gnu packages glib) dbus))
      ("expat" ,(@ (gnu packages xml) expat))
      ("pango" ,(@ (gnu packages gtk) pango))
      ("cairo" ,(@ (gnu packages gtk) cairo))
      ("atk" ,(@ (gnu packages gtk) atk))
      ("gtk+" ,(@ (gnu packages gtk) gtk+))
      ("gdk-pixbuf" ,(@ (gnu packages gtk) gdk-pixbuf))
      ("pulseaudio" ,(@ (gnu packages pulseaudio) pulseaudio))
      ("fontconfig" ,(@ (gnu packages fontutils) fontconfig))
      ("bash" ,(@ (gnu packages bash) bash))
      ("glibc" ,(@ (gnu packages base) glibc))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((output (assoc-ref %outputs "out"))
               (source (assoc-ref %build-inputs "source"))
                                        ;(working-dir (string-append (getcwd) "/package"))
               (working-dir output)
               (ar (string-append (assoc-ref %build-inputs "binutils") "/bin/ar"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (patchelf (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf")))

          ;; Extraction phase
          (mkdir-p working-dir)
          (setenv "PATH" (string-append (assoc-ref %build-inputs "xz") "/bin"))
          (zero? (system* ar "x" source "data.tar.xz"))
          (zero? (system* tar "xvf" "data.tar.xz" "-C" working-dir "./opt"))

          ;; Patching phase
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/brave.com/brave/brave"))
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/brave.com/brave/chrome_crashpad_handler"))
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/brave.com/brave/chrome-sandbox"))

          ;; Wrapping phase - give it libraries and some other envars
          (setenv "PATH" (string-append (assoc-ref %build-inputs "bash") "/bin"))
          (wrap-program (string-append working-dir "/opt/brave.com/brave/brave")
			`("LD_LIBRARY_PATH" ":" prefix ("/run/current-system/profile/lib")))
          (wrap-program (string-append working-dir "/opt/brave.com/brave/brave")
			`("LD_LIBRARY_PATH" ":" prefix (,(string-append
							  (assoc-ref %build-inputs "nss")
							  "/lib/nss"))))
          (wrap-program (string-append working-dir "/opt/brave.com/brave/brave")
			`("LD_LIBRARY_PATH" ":" prefix
			  ,(map (lambda (input)
				  (string-append (assoc-ref %build-inputs input)
						 "/lib"))
				'("libxcomposite" "libxtst" "nss" "nspr"
				  "cups" "libxrandr" "libxscrnsaver" "alsa-lib"
				  "gcc" "libxcursor" "libxdamage"
				  "gobject-introspection" "glib" "dbus" "expat"
				  "pango" "cairo" "atk" "gtk+" "gdk-pixbuf"
				  "pulseaudio")))
			;; Reads fontconfig config (version 1.12.6 compatible required)
			`("FONTCONFIG_PATH" ":" prefix (,(string-append
							  (assoc-ref %build-inputs "fontconfig")
							  "/etc/fonts"))))

          ;; Polishing phase
          (mkdir-p (string-append working-dir "/bin"))
          (symlink
           "../opt/brave.com/brave/brave"
           (string-append working-dir "/bin/brave"))

          (with-directory-excursion (string-append working-dir "/opt/brave.com/brave")
				    (for-each
				     (lambda (size)
				       (let ((icons (string-append output "/share/icons/hicolor/"
								   size "x" size "/apps")))
					 (mkdir-p icons)
					 (copy-file (string-append "product_logo_" size ".png")
						    (string-append icons "/brave.png"))))
				     '("24" "48" "64" "128" "256")))

          (mkdir-p (string-append (assoc-ref %outputs "out") "/share/applications"))
          (with-output-to-file
              (string-append (assoc-ref %outputs "out") "/share/applications/brave.desktop")
            (lambda _
              (format #t "[Desktop Entry]~@
                    Name=Brave~@
                    Comment=Browse 3x faster than Chrome~@
                    Exec=~a/bin/brave~@
                    Icon=brave~@
                    Categories=Network~@
                    Type=Application~%"
                      (assoc-ref %outputs "out"))))
          ))))
   (home-page "https://www.google.com")
   (synopsis "Google Chrome web browser.")
   (description "Google Chrome web browser.")
   (license #f)))

;; Downloads and wraps Chrome
;; latest version txt files at https://omahaproxy.appspot.com/all?os=linux&channel=stable
;; https://github.com/voidlinux/void-packages/blob/master/srcpkgs/google-chrome/template
;; guix environment --ad-hoc libxcomposite libxtst nss nspr cups libxrandr libxscrnsaver alsa-lib gcc:lib libxcursor libxdamage gobject-introspection glib dbus expat pango cairo atk gtk+ gdk-pixbuf -- sh -c "LD_LIBRARY_PATH=\$GUIX_ENVIRONMENT/lib:\$GUIX_ENVIRONMENT/lib/nss ./chrome"
;; FONTCONFIG_PATH=$GUIX_ENVIRONMENT/etc/fonts LD_LIBRARY_PATH=$GUIX_ENVIRONMENT/lib:$GUIX_ENVIRONMENT/lib/nss:$(pwd):$(guix build fontconfig)/lib:/run/current-system/profile/lib ./chrome netflix.com
(define-public google-chrome
  (package
   (name "google-chrome")
   (version "94.0.4606.61")
   (source
    (origin
     (method url-fetch)
     (uri "https://dl.google.com/linux/chrome/deb/pool/main/g/google-chrome-stable/google-chrome-stable_94.0.4606.61-1_amd64.deb")
     (sha256
      (base32
       "116xrf8hcprbdpdx6a4xysac2phyvw88vs3n1bs24ly6pxydsasz"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("patchelf" ,(@ (gnu packages elf) patchelf))
      ("tar" ,(@ (gnu packages base) tar))
      ("xz" ,(@ (gnu packages compression) xz))
      ("binutils" ,(@ (gnu packages base) binutils))))
   (inputs
    `(("libxcomposite" ,(@ (gnu packages xorg) libxcomposite))
      ("libxtst" ,(@ (gnu packages xorg) libxtst))
      ("nss" ,(@ (gnu packages nss) nss))
      ("nspr" ,(@ (gnu packages nss) nspr))
      ("cups" ,(@ (gnu packages cups) cups))
      ("libxrandr" ,(@ (gnu packages xorg) libxrandr))
      ("libxscrnsaver" ,(@ (gnu packages xorg) libxscrnsaver))
      ("alsa-lib" ,(@ (gnu packages linux) alsa-lib))
      ("gcc" ,(@ (gnu packages gcc) gcc-7) "lib")
      ("libxcursor" ,(@ (gnu packages xorg) libxcursor))
      ("libxdamage" ,(@ (gnu packages xorg) libxdamage))
      ("gobject-introspection" ,(@ (gnu packages glib) gobject-introspection))
      ("glib" ,(@ (gnu packages glib) glib))
      ("dbus" ,(@ (gnu packages glib) dbus))
      ("expat" ,(@ (gnu packages xml) expat))
      ("pango" ,(@ (gnu packages gtk) pango))
      ("cairo" ,(@ (gnu packages gtk) cairo))
      ("atk" ,(@ (gnu packages gtk) atk))
      ("gtk+" ,(@ (gnu packages gtk) gtk+))
      ("gdk-pixbuf" ,(@ (gnu packages gtk) gdk-pixbuf))
      ("pulseaudio" ,(@ (gnu packages pulseaudio) pulseaudio))
      ("fontconfig" ,(@ (gnu packages fontutils) fontconfig))
      ("bash" ,(@ (gnu packages bash) bash))
      ("glibc" ,(@ (gnu packages base) glibc))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((output (assoc-ref %outputs "out"))
               (source (assoc-ref %build-inputs "source"))
                                        ;(working-dir (string-append (getcwd) "/package"))
               (working-dir output)
               (ar (string-append (assoc-ref %build-inputs "binutils") "/bin/ar"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (patchelf (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf")))

          ;; Extraction phase
          (mkdir-p working-dir)
          (setenv "PATH" (string-append (assoc-ref %build-inputs "xz") "/bin"))
          (zero? (system* ar "x" source "data.tar.xz"))
          (zero? (system* tar "xvf" "data.tar.xz" "-C" working-dir "./opt"))

          ;; Patching phase
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/google/chrome/chrome"))
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/google/chrome/nacl_helper"))
          (invoke patchelf "--set-interpreter"
                  (string-append (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2")
                  (string-append working-dir "/opt/google/chrome/chrome_crashpad_handler"))

          ;; Wrapping phase - give it libraries and some other envars
          (setenv "PATH" (string-append (assoc-ref %build-inputs "bash") "/bin"))
          (wrap-program (string-append working-dir "/opt/google/chrome/chrome")
			`("LD_LIBRARY_PATH" ":" prefix ("/run/current-system/profile/lib")))
          (wrap-program (string-append working-dir "/opt/google/chrome/chrome")
			`("LD_LIBRARY_PATH" ":" prefix (,(string-append
							  (assoc-ref %build-inputs "nss")
							  "/lib/nss"))))
          (wrap-program (string-append working-dir "/opt/google/chrome/chrome")
			`("LD_LIBRARY_PATH" ":" prefix
			  ,(map (lambda (input)
				  (string-append (assoc-ref %build-inputs input)
						 "/lib"))
				'("libxcomposite" "libxtst" "nss" "nspr"
				  "cups" "libxrandr" "libxscrnsaver" "alsa-lib"
				  "gcc" "libxcursor" "libxdamage"
				  "gobject-introspection" "glib" "dbus" "expat"
				  "pango" "cairo" "atk" "gtk+" "gdk-pixbuf"
				  "pulseaudio")))
			;; Reads fontconfig config (version 1.12.6 compatible required)
			`("FONTCONFIG_PATH" ":" prefix (,(string-append
							  (assoc-ref %build-inputs "fontconfig")
							  "/etc/fonts"))))

          ;; Polishing phase
          (mkdir-p (string-append working-dir "/bin"))
          (symlink
           "../opt/google/chrome/chrome"
           (string-append working-dir "/bin/chrome"))

          (with-directory-excursion (string-append working-dir "/opt/google/chrome")
				    (for-each
				     (lambda (size)
				       (let ((icons (string-append output "/share/icons/hicolor/"
								   size "x" size "/apps")))
					 (mkdir-p icons)
					 (copy-file (string-append "product_logo_" size ".png")
						    (string-append icons "/chrome.png"))))
				     '("24" "48" "64" "128" "256")))

          (mkdir-p (string-append (assoc-ref %outputs "out") "/share/applications"))
          (with-output-to-file
              (string-append (assoc-ref %outputs "out") "/share/applications/chrome.desktop")
            (lambda _
              (format #t
                      "[Desktop Entry]~@
                    Name=Chrome~@
                    Comment=Surf the Web to find Nora~@
                    Exec=~a/bin/chrome~@
                    Icon=chrome~@
                    Categories=Network~@
                    Type=Application~%"
                      (assoc-ref %outputs "out"))))
          ))))
   (home-page "https://www.google.com")
   (synopsis "Google Chrome web browser.")
   (description "Google Chrome web browser.")
   (license #f)))


(define-public font-modernsuite
  (package
   (name "font-modernsuite")
   (version "0.0.1")
   (source (origin
            (method url-fetch/zipbomb)
            (uri (string-append "https://files.catbox.moe/8314ww.zip"))
            (sha256
             (base32
              "0z17kzk0d6zlka8v9bd5965y2230yhzmild7fsxyl75hlj8mn5p8"))))
   (build-system font-build-system)
   (home-page "https://shinntype.com/typefaces/scotch-modern/")
   (synopsis "Scotch Modern and Figgins Sans typefaces")
   (description "The two faces which comprise the Modern Suite are based on types from the middle of the 19th century.")
   (license (nonfree "http://shinntype.com/wp-content/uploads/files/pdf/Scotch_Modern.pdf"))))

(define-public font-pragmatapro
  (package
   (name "font-pragmatapro")
   (version "0.829")
   (source (origin
            (method url-fetch/zipbomb)
            (uri (string-append "https://files.catbox.moe/3nh2tk.zip"))
            (sha256
             (base32
              "1njp0xwk9kkf9djds6r8ihyc5bh58hkgxggawx6r3sj98fg58wss"))))
   (build-system font-build-system)
   (home-page "https://fsd.it/shop/fonts/pragmatapro/")
   (synopsis "PragmataPro monospace typeface")
   (description
    "PragmataPro™ is a condensed monospaced font optimized for screen, designed by Fabrizio Schiavi to be the ideal font for coding, math and engineering.")
   (license (nonfree "https://fsd.it/shop/terms/"))))

(define-public font-berlin
  (package
   (name "font-berlin")
   (version "1")
   (source (origin
            (method url-fetch/zipbomb)
            (uri "http://www.enpassant.dk/chess/downl/berlin.zip")
            (sha256
             (base32
              "1g0zjhn69381ssnlkc6jvgh263sz2lgq477cfrywck9dr68xnlsp"))))
   (build-system font-build-system)
   (home-page "http://www.enpassant.dk/chess/fonteng.htm")
   (synopsis "True Type Font for chess diagrams and figurine notation")
   (description "True Type Font by Eric Bentzen for diagrams and figurine notation. Based on the familiar design from the East German \"Sportverlag\", that published so many popular chess books. Support for fairy chess diagrams.")
   (license (nonfree "http://www.enpassant.dk/chess/fonteng.htm")))) ; TODO

(define-public font-joypixels
  (package
   (name "font-joypixels")
   (version "6.6.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://cdn.joypixels.com/arch-linux/font/"
				version
				"/joypixels-android.ttf"))
            (sha256
             (base32
              "17gjaz7353zyprmds64p01qivy2r8pwf88nvvhi57idas2qd604n"))))
   (build-system font-build-system)
   (home-page "https://www.joypixels.com")
   (synopsis "Emoji as a Service")
   (description "An emoji font with originally crafted icon designs.")
   (license (nonfree "https://cdn.joypixels.com/arch-linux/license/free-license.pdf"))))

;; (define-public font-sf-pro
;;   (package
;;     (name "font-sf-pro")
;;     (version "17.0d9e1")
;;     (source (origin
;;               (method url-fetch/zipbomb)
;;               (uri (string-append "https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg"))
;;               (sha256
;;                (base32
;;                 "1wy3v2c87cpd9w333w78s6nn7fl5cnbsv8wff01xml6m3wgl7brz"))))
;;     (build-system font-build-system)
;;    (native-inputs `(("p7zip" ,p7zip)))
;;     (home-page "https://developer.apple.com/fonts/")
;;     (synopsis "SF Pro sans-serif typeface")
;;     (description
;;      "This neutral, flexible, sans-serif typeface is the system font for iOS, iPad OS, macOS and tvOS. SF Pro features nine weights, variable optical sizes for optimal legibility, and includes a rounded variant. SF Pro supports over 150 languages across Latin, Greek, and Cyrillic scripts.")
;;     (license (nonfree "https://www.apple.com/legal/sla/"))))

(operating-system
 (host-name "andermatt")
 (timezone "Europe/Berlin")
 (locale "en_US.utf8")
 (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "latitude" #:options '("compose:menu" "rupeesign:4")))
 (kernel linux)
 (initrd (lambda (file-systems . rest)
	   (apply microcode-initrd file-systems
		  #:initrd base-initrd
		  #:microcode-packages (list intel-microcode)
		  rest)))
 (firmware (cons* iwlwifi-firmware
                  %base-firmware))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))))
 (file-systems (cons* (file-system
                       (device (uuid "6102df92-ed4b-44be-8392-5530097ae0f8" 'btrfs))
                       (mount-point "/")
                       (flags '(no-atime))
                       (needed-for-boot? #t)
                       (options "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@guix/@")
                       (type "btrfs"))
                      (file-system
                       (device (uuid "6102df92-ed4b-44be-8392-5530097ae0f8" 'btrfs))
                       (mount-point "/gnu")
                       (flags '(no-atime))
                       (needed-for-boot? #f)
                       (options "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@guix/@gnu")
                       (type "btrfs"))
                      (file-system
                       (device (uuid "6102df92-ed4b-44be-8392-5530097ae0f8" 'btrfs))
                       (mount-point "/home")
                       (flags '(no-atime))
                       (needed-for-boot? #f)
                       (options "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@guix/@home")
                       (type "btrfs"))
                      (file-system
                       (device (uuid "6102df92-ed4b-44be-8392-5530097ae0f8" 'btrfs))
                       (mount-point "/var/log")
                       (flags '(no-atime))
                       (needed-for-boot? #t)
                       (options "compress-force=zstd:6,user_subvol_rm_allowed,discard=async,subvol=/@guix/@var-log")
                       (type "btrfs"))
                      (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "C3EE-5047" 'fat32))
                       (type "vfat"))
                      %base-file-systems))

 ;; This is where user accounts are specified.  The "root"
 ;; account is implicit, and is initially created with the
 ;; empty password.
 (users (cons (user-account
               (name "xha")
               (group "users")
               (supplementary-groups '("wheel" "audio" "video" "input" "tty" "netdev")))
              %base-user-accounts))

 ;; Globally-installed packages.
 (packages (cons* alsa-utils
		  bemenu
		  bluez
		  brave
		  btrfs-progs
		  conky
                  curl
		  dmenu
		  emacs-next
		  emacs-evil
		  emacs-ivy
		  emacs-rainbow-delimiters
		  emacs-typescript-mode
		  emacs-web-mode
		  font-adobe-source-code-pro
		  font-berlin
		  font-google-noto
		  font-joypixels
		  font-modernsuite
		  font-pragmatapro
		  foot
		  gimp
		  git
		  google-chrome
		  grim
		  imv
		  mpv
		  neofetch
		  nss-certs
		  p7zip
		  pavucontrol
		  ;;		   pipewire
		  slurp
		  sway
		  tmux
		  vim
                  youtube-dl
		  zathura
		  zathura-cb
		  zathura-djvu
		  zathura-pdf-mupdf
		  zathura-ps
		  %base-packages))

 (services (append (list fontconfig-file-system-service
                         (service network-manager-service-type)
                         (service wpa-supplicant-service-type)
                         (elogind-service)
                         (service ntp-service-type
                                  (ntp-configuration
                                   (allow-large-adjustment? #t)))
			 (service pulseaudio-service-type)
			 (service bluetooth-service-type)
                         (service alsa-service-type)
                         (service openssh-service-type
                                  (openssh-configuration
                                   (openssh openssh-sans-x)
                                   (port-number 2222))))
                   %base-services)))

